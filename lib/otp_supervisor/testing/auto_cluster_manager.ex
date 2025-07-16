defmodule OTPSupervisor.Testing.AutoClusterManager do
  @moduledoc """
  Manages automatic cluster lifecycle for distributed testing.

  Integrates with the existing TestCluster.Manager but adds:
  - Automatic startup/shutdown
  - Cluster reuse across test runs
  - Configuration management
  - Error handling and recovery

  This GenServer coordinates between test requirements and cluster resources,
  ensuring optimal cluster usage while maintaining reliability.
  """

  use GenServer
  require Logger

  alias OTPSupervisor.TestCluster.{Manager, Diagnostics}
  alias OTPSupervisor.Testing.Config

  @default_timeout 60_000
  @cluster_cleanup_timeout 10_000

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Start or reuse a cluster for the given test requirements.

  Returns cluster information if successful, or error with diagnosis.
  """
  def start_cluster_for_tests(requirements) do
    GenServer.call(__MODULE__, {:start_cluster_for_tests, requirements}, @default_timeout)
  end

  @doc """
  Get current cluster information and status.
  """
  def get_cluster_info do
    GenServer.call(__MODULE__, :get_cluster_info)
  end

  @doc """
  Clean up cluster if it was managed by this process.
  """
  def cleanup_if_managed do
    GenServer.call(__MODULE__, :cleanup_if_managed, @cluster_cleanup_timeout)
  end

  @doc """
  Force cleanup of all clusters (for emergency situations).
  """
  def force_cleanup_all do
    GenServer.call(__MODULE__, :force_cleanup_all, @cluster_cleanup_timeout)
  end

  @doc """
  Check if a cluster is currently available and suitable for requirements.
  """
  def check_cluster_availability(requirements) do
    GenServer.call(__MODULE__, {:check_cluster_availability, requirements})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Logger.info("Starting AutoClusterManager")

    state = %{
      cluster_info: nil,
      managed: false,
      last_requirements: nil,
      startup_time: nil,
      config: load_config(opts)
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:start_cluster_for_tests, requirements}, _from, state) do
    Logger.info("Starting cluster for test requirements: #{inspect(requirements)}")

    case determine_cluster_strategy(requirements, state) do
      {:reuse_existing, cluster_info} ->
        Logger.info("Reusing existing cluster with #{length(cluster_info.nodes)} nodes")

        new_state = %{
          state
          | cluster_info: cluster_info,
            managed: false,
            last_requirements: requirements
        }

        {:reply, {:ok, cluster_info}, new_state}

      {:start_new, cluster_opts} ->
        case start_new_cluster(cluster_opts, requirements) do
          {:ok, cluster_info} ->
            Logger.info("Started new cluster with #{length(cluster_info.nodes)} nodes")

            new_state = %{
              state
              | cluster_info: cluster_info,
                managed: true,
                last_requirements: requirements,
                startup_time: DateTime.utc_now()
            }

            {:reply, {:ok, cluster_info}, new_state}

          {:error, reason} ->
            diagnosis = handle_cluster_startup_failure(reason, requirements, state)
            {:reply, {:error, diagnosis}, state}
        end

      {:skip_cluster, reason} ->
        Logger.info("Skipping cluster startup: #{reason}")
        cluster_info = %{cluster_active: false, nodes: [], reason: reason}
        new_state = %{state | cluster_info: cluster_info, managed: false}
        {:reply, {:ok, cluster_info}, new_state}
    end
  end

  @impl true
  def handle_call(:get_cluster_info, _from, state) do
    cluster_info = build_cluster_info(state)
    {:reply, cluster_info, state}
  end

  @impl true
  def handle_call(:cleanup_if_managed, _from, state) do
    if state.managed and state.cluster_info do
      Logger.info("Cleaning up managed cluster")

      case cleanup_cluster(state.cluster_info) do
        :ok ->
          new_state = %{state | cluster_info: nil, managed: false, startup_time: nil}
          {:reply, :ok, new_state}

        {:error, reason} ->
          Logger.warning("Cluster cleanup had issues: #{inspect(reason)}")
          # Still mark as cleaned up to avoid resource leaks
          new_state = %{state | cluster_info: nil, managed: false, startup_time: nil}
          {:reply, {:warning, reason}, new_state}
      end
    else
      Logger.debug("No managed cluster to clean up")
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call(:force_cleanup_all, _from, state) do
    Logger.warning("Force cleaning up all clusters")

    # Stop the underlying TestCluster.Manager
    case Manager.stop_cluster() do
      :ok -> Logger.info("Successfully stopped all clusters")
      {:error, reason} -> Logger.warning("Issues stopping clusters: #{inspect(reason)}")
    end

    new_state = %{state | cluster_info: nil, managed: false, startup_time: nil}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:check_cluster_availability, requirements}, _from, state) do
    availability = check_existing_cluster_suitability(requirements)
    {:reply, availability, state}
  end

  # Private implementation

  defp load_config(opts) do
    Config.load_config(opts)
  end

  defp determine_cluster_strategy(requirements, state) do
    # ENFORCE REAL CLUSTER REQUIREMENTS - NO BYPASSES
    cond do
      not requirements.needs_cluster ->
        # Only skip if tests truly don't need a cluster
        {:skip_cluster, "no cluster needed for these tests"}

      state.config.reuse_clusters and can_reuse_existing_cluster?(requirements) ->
        case get_existing_cluster_info() do
          {:ok, cluster_info} -> {:reuse_existing, cluster_info}
          {:error, _} -> {:start_new, build_cluster_opts(requirements, state)}
        end

      true ->
        # Always start new cluster for distributed tests - no bypasses
        {:start_new, build_cluster_opts(requirements, state)}
    end
  end

  defp can_reuse_existing_cluster?(requirements) do
    case check_existing_cluster_suitability(requirements) do
      {:ok, :suitable} -> true
      _ -> false
    end
  end

  defp check_existing_cluster_suitability(requirements) do
    case Manager.get_status() do
      {:ok, %{overall: :running, nodes: nodes}} ->
        if length(Map.keys(nodes)) >= requirements.min_cluster_size do
          {:ok, :suitable}
        else
          {:error, :insufficient_size}
        end

      {:ok, %{overall: status}} ->
        {:error, {:not_running, status}}

      {:error, reason} ->
        {:error, {:status_check_failed, reason}}
    end
  end

  defp get_existing_cluster_info do
    case Manager.get_status() do
      {:ok, status} ->
        cluster_info = %{
          cluster_active: status.overall == :running,
          nodes: Map.keys(status.nodes),
          cluster_size: length(Map.keys(status.nodes)),
          managed_by: :existing,
          status: status
        }

        {:ok, cluster_info}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_cluster_opts(requirements, state) do
    cluster_size = determine_cluster_size(requirements, state)

    [
      node_count: cluster_size,
      startup_timeout: state.config.cluster_startup_timeout,
      managed_by: :auto_test_manager
    ]
  end

  defp determine_cluster_size(requirements, state) do
    cond do
      state.config.ci_mode ->
        min(requirements.min_cluster_size, state.config.ci_cluster_size)

      requirements.min_cluster_size > state.config.max_cluster_size ->
        Logger.warning(
          "Requested cluster size #{requirements.min_cluster_size} exceeds max #{state.config.max_cluster_size}, using max"
        )

        state.config.max_cluster_size

      true ->
        max(requirements.min_cluster_size, state.config.default_cluster_size)
    end
  end

  defp start_new_cluster(cluster_opts, requirements) do
    Logger.info("Starting new cluster with options: #{inspect(cluster_opts)}")

    case Manager.start_cluster(cluster_opts) do
      {:ok, nodes} ->
        cluster_info = %{
          cluster_active: true,
          nodes: Map.keys(nodes),
          cluster_size: length(Map.keys(nodes)),
          managed_by: :auto_cluster_manager,
          startup_time: DateTime.utc_now(),
          requirements: requirements,
          raw_nodes: nodes
        }

        {:ok, cluster_info}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_cluster_startup_failure(reason, _requirements, state) do
    Logger.error("Cluster startup failed: #{inspect(reason)}")

    diagnosis = Diagnostics.diagnose_startup_failure(reason)

    # NO FALLBACK STRATEGIES - FAIL HARD FOR DISTRIBUTED TESTS
    %{
      problem:
        diagnosis.problem ||
          "Cluster startup failed - distributed tests cannot run without a cluster",
      reason: reason,
      solutions: diagnosis.solutions || [],
      # Always fail hard
      fallback_strategy: :fail_hard,
      # Never skip distributed tests
      can_skip_tests: false,
      retry_suggestions: build_retry_suggestions(reason, state)
    }
  end

  defp build_retry_suggestions(reason, _state) do
    base_suggestions = [
      "Check system resources (memory, CPU)",
      "Ensure no other test clusters are running",
      "Verify network connectivity"
    ]

    specific_suggestions =
      case reason do
        {:port_allocation_failed, _} ->
          ["Try different port ranges", "Stop other development servers"]

        {:node_connection_failed, _} ->
          ["Check firewall settings", "Verify hostname resolution"]

        _ ->
          []
      end

    base_suggestions ++ specific_suggestions
  end

  defp cleanup_cluster(cluster_info) do
    if cluster_info.managed_by == :auto_cluster_manager do
      Logger.info("Cleaning up auto-managed cluster")
      Manager.stop_cluster()
    else
      Logger.debug("Cluster not managed by us, skipping cleanup")
      :ok
    end
  end

  defp build_cluster_info(state) do
    case state.cluster_info do
      nil ->
        %{cluster_active: false, nodes: [], managed: false}

      cluster_info ->
        Map.merge(cluster_info, %{
          managed: state.managed,
          uptime: calculate_uptime(state.startup_time),
          last_requirements: state.last_requirements
        })
    end
  end

  defp calculate_uptime(nil), do: nil

  defp calculate_uptime(startup_time) do
    DateTime.diff(DateTime.utc_now(), startup_time, :second)
  end
end
