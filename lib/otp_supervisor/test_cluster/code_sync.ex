defmodule OTPSupervisor.TestCluster.CodeSync do
  @moduledoc """
  Ensures all test nodes run the same code version.

  Features:
  - Compile and sync code to all nodes
  - Validate module versions across cluster
  - Hot code reloading for test nodes
  - Prevent testing against stale code

  This addresses the critical issue of accidentally testing against
  old code from previously running servers.
  """

  require Logger

  @sync_timeout 30_000
  @key_modules [
    OTPSupervisor.Distributed.ClusterStateManager,
    OTPSupervisor.Distributed.ToolManager,
    OTPSupervisor.Distributed.SingleNodeSimulator,
    OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth,
    OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology,
    OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList,
    OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo
  ]

  @doc """
  Synchronize code to all nodes in the cluster.

  This ensures that all test nodes are running the exact same
  code version, preventing issues with stale code.
  """
  def sync_code_to_cluster(nodes) when is_list(nodes) do
    Logger.info("Synchronizing code to #{length(nodes)} nodes")

    with :ok <- ensure_code_compiled(),
         :ok <- sync_beam_files_to_nodes(nodes),
         :ok <- validate_code_versions(nodes),
         :ok <- ensure_applications_started(nodes) do
      Logger.info("Code synchronization completed successfully")
      :ok
    else
      {:error, reason} ->
        Logger.error("Code synchronization failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Validate that all nodes have the same code versions.
  """
  def validate_code_versions(nodes) when is_list(nodes) do
    Logger.debug("Validating code versions across #{length(nodes)} nodes")

    case check_module_versions(nodes, @key_modules) do
      :ok ->
        Logger.debug("All nodes have consistent code versions")
        :ok

      {:error, mismatches} ->
        Logger.error("Code version mismatches detected: #{inspect(mismatches)}")
        {:error, {:version_mismatches, mismatches}}
    end
  end

  @doc """
  Get code version information from all nodes.
  """
  def get_cluster_code_info(nodes) when is_list(nodes) do
    Logger.debug("Gathering code information from cluster")

    code_info =
      nodes
      |> Enum.map(fn node ->
        {node, get_node_code_info(node)}
      end)
      |> Enum.into(%{})

    {:ok, code_info}
  end

  # Private implementation

  defp ensure_code_compiled do
    Logger.debug("Ensuring code is compiled...")

    case System.cmd("mix", ["compile"], stderr_to_stdout: true) do
      {output, 0} ->
        Logger.debug("Code compilation successful")

        if String.contains?(output, "warning") do
          Logger.warning("Compilation warnings detected:\n#{output}")
        end

        :ok

      {output, exit_code} ->
        Logger.error("Code compilation failed (exit #{exit_code}):\n#{output}")
        {:error, {:compilation_failed, exit_code, output}}
    end
  end

  defp sync_beam_files_to_nodes(nodes) do
    Logger.debug("Syncing BEAM files to nodes...")

    # Get the current code paths
    code_paths = :code.get_path()

    # Sync each node
    sync_results =
      nodes
      |> Enum.map(fn node ->
        case sync_code_to_node(node, code_paths) do
          :ok -> {node, :ok}
          {:error, reason} -> {node, {:error, reason}}
        end
      end)

    failed_syncs =
      sync_results
      |> Enum.filter(fn {_, result} -> result != :ok end)

    if Enum.empty?(failed_syncs) do
      :ok
    else
      {:error, {:sync_failed, failed_syncs}}
    end
  end

  defp sync_code_to_node(node, code_paths) do
    Logger.debug("Syncing code to node: #{node}")

    try do
      # Add code paths to the remote node
      case :rpc.call(node, :code, :set_path, [code_paths], @sync_timeout) do
        true ->
          # Ensure key modules are loaded
          load_key_modules_on_node(node)

        {:badrpc, reason} ->
          {:error, {:code_path_sync_failed, reason}}

        false ->
          {:error, :code_path_sync_rejected}
      end
    rescue
      error ->
        {:error, {:sync_crashed, error}}
    end
  end

  defp load_key_modules_on_node(node) do
    Logger.debug("Loading key modules on node: #{node}")

    # Ensure our key modules are loaded and up-to-date
    load_results =
      @key_modules
      |> Enum.map(fn module ->
        case :rpc.call(node, :code, :ensure_loaded, [module], 10_000) do
          {:module, ^module} ->
            {module, :ok}

          {:error, reason} ->
            {module, {:error, reason}}

          {:badrpc, reason} ->
            {module, {:error, {:rpc_failed, reason}}}
        end
      end)

    failed_loads =
      load_results
      |> Enum.filter(fn {_, result} -> result != :ok end)

    if Enum.empty?(failed_loads) do
      :ok
    else
      Logger.warning("Some modules failed to load on #{node}: #{inspect(failed_loads)}")
      # Don't fail the sync for this - modules might load later
      :ok
    end
  end

  defp check_module_versions(nodes, modules) do
    Logger.debug("Checking module versions across nodes...")

    version_mismatches =
      modules
      |> Enum.flat_map(fn module ->
        case check_single_module_versions(nodes, module) do
          :ok -> []
          {:error, mismatch_info} -> [{module, mismatch_info}]
        end
      end)

    if Enum.empty?(version_mismatches) do
      :ok
    else
      {:error, version_mismatches}
    end
  end

  defp check_single_module_versions(nodes, module) do
    # Get module version/hash from each node
    module_versions =
      nodes
      |> Enum.map(fn node ->
        case get_module_version(node, module) do
          {:ok, version} -> {node, version}
          {:error, reason} -> {node, {:error, reason}}
        end
      end)

    # Extract successful versions
    successful_versions =
      module_versions
      |> Enum.filter(fn {_, version} -> not match?({:error, _}, version) end)
      |> Enum.map(fn {node, version} -> {node, version} end)

    if length(successful_versions) < length(nodes) do
      # Some nodes failed to provide version info
      failed_nodes =
        module_versions
        |> Enum.filter(fn {_, version} -> match?({:error, _}, version) end)

      {:error, {:version_check_failed, failed_nodes}}
    else
      # Check if all versions are the same
      version_hashes = Enum.map(successful_versions, fn {_, version} -> version end)
      unique_versions = Enum.uniq(version_hashes)

      if length(unique_versions) <= 1 do
        :ok
      else
        {:error, {:version_mismatch, successful_versions}}
      end
    end
  end

  defp get_module_version(node, module) do
    case :rpc.call(node, :code, :get_object_code, [module], 10_000) do
      {:badrpc, reason} ->
        {:error, {:rpc_failed, reason}}

      {^module, binary, _filename} ->
        # Create a hash of the binary to represent the version
        hash = :crypto.hash(:md5, binary) |> Base.encode16()
        {:ok, hash}

      :error ->
        {:error, :module_not_found}
    end
  end

  defp ensure_applications_started(nodes) do
    Logger.debug("Ensuring applications are started on all nodes...")

    # Applications that should be running on test nodes
    required_apps = [:logger, :otp_supervisor]

    app_results =
      nodes
      |> Enum.map(fn node ->
        case ensure_apps_on_node(node, required_apps) do
          :ok -> {node, :ok}
          {:error, reason} -> {node, {:error, reason}}
        end
      end)

    failed_apps =
      app_results
      |> Enum.filter(fn {_, result} -> result != :ok end)

    if Enum.empty?(failed_apps) do
      :ok
    else
      Logger.warning("Some applications failed to start: #{inspect(failed_apps)}")
      # Don't fail the sync for this - apps might start later or not be needed
      :ok
    end
  end

  defp ensure_apps_on_node(node, apps) do
    Logger.debug("Starting applications on #{node}: #{inspect(apps)}")

    start_results =
      apps
      |> Enum.map(fn app ->
        case :rpc.call(node, Application, :ensure_all_started, [app], 15_000) do
          {:ok, _started_apps} ->
            {app, :ok}

          {:error, reason} ->
            {app, {:error, reason}}

          {:badrpc, reason} ->
            {app, {:error, {:rpc_failed, reason}}}
        end
      end)

    failed_starts =
      start_results
      |> Enum.filter(fn {_, result} -> result != :ok end)

    if Enum.empty?(failed_starts) do
      :ok
    else
      {:error, {:app_start_failed, failed_starts}}
    end
  end

  defp get_node_code_info(node) do
    try do
      # Get basic node information
      case :rpc.call(node, :erlang, :system_info, [:system_version], 5000) do
        {:badrpc, reason} ->
          %{error: {:rpc_failed, reason}}

        system_version ->
          # Get module information for key modules
          module_info =
            @key_modules
            |> Enum.map(fn module ->
              case get_module_version(node, module) do
                {:ok, version} -> {module, version}
                {:error, reason} -> {module, {:error, reason}}
              end
            end)
            |> Enum.into(%{})

          %{
            system_version: system_version,
            node: node,
            modules: module_info,
            timestamp: DateTime.utc_now()
          }
      end
    rescue
      error ->
        %{error: {:info_collection_failed, error}}
    end
  end
end
