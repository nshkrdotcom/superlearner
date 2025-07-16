defmodule OTPSupervisor.TestCluster.HostnameResolver do
  @moduledoc """
  Simple hostname resolution for WSL Ubuntu 24.04 and other environments.

  Provides a fallback strategy that tries different hostname resolution methods
  in order until one works:
  1. System hostname (from :inet.gethostname/0)
  2. localhost
  3. 127.0.0.1

  This addresses WSL networking issues where hostname resolution can be
  inconsistent or fail entirely.
  """

  require Logger

  @doc """
  Get the best available hostname for cluster operations.

  Returns {:ok, hostname} or {:error, reason}.

  ## Examples

      iex> OTPSupervisor.TestCluster.HostnameResolver.get_cluster_hostname()
      {:ok, "localhost"}

      iex> OTPSupervisor.TestCluster.HostnameResolver.get_cluster_hostname()
      {:ok, "my-hostname"}
  """
  def get_cluster_hostname do
    Logger.debug("Starting hostname resolution for cluster")

    # Try strategies in order until one works
    # For distributed Erlang with longnames, prefer IP addresses and localhost
    strategies = [
      &try_ip_address/0,      # 127.0.0.1 - best for distributed Erlang longnames
      &try_localhost/0,       # localhost - good fallback
      &try_system_hostname/0  # system hostname - last resort (may not work with longnames)
    ]

    Enum.reduce_while(strategies, {:error, :no_hostname}, fn strategy, _acc ->
      case strategy.() do
        {:ok, hostname} ->
          Logger.info("Successfully resolved hostname: #{hostname}")
          {:halt, {:ok, hostname}}

        {:error, reason} ->
          Logger.debug("Hostname strategy failed: #{inspect(reason)}")
          {:cont, {:error, :strategy_failed}}
      end
    end)
  end

  @doc """
  Test if a hostname can be resolved and is reachable.

  Returns {:ok, hostname} if successful, {:error, reason} otherwise.
  """
  def test_hostname_resolution(hostname) when is_binary(hostname) do
    hostname_charlist = String.to_charlist(hostname)

    case :inet.gethostbyname(hostname_charlist) do
      {:ok, _hostent} ->
        Logger.debug("Hostname #{hostname} resolved successfully")
        {:ok, hostname}

      {:error, reason} ->
        Logger.debug("Hostname #{hostname} resolution failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  # Private functions implementing the fallback strategy

  defp try_system_hostname do
    Logger.debug("Trying system hostname resolution")

    case :inet.gethostname() do
      {:ok, hostname_charlist} ->
        hostname_str = List.to_string(hostname_charlist)
        Logger.debug("System hostname: #{hostname_str}")

        # Verify the hostname actually resolves
        case test_hostname_resolution(hostname_str) do
          {:ok, hostname} -> {:ok, hostname}
          {:error, _} -> {:error, :hostname_not_resolvable}
        end

      {:error, reason} ->
        Logger.debug("Failed to get system hostname: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp try_localhost do
    Logger.debug("Trying localhost resolution")

    case test_hostname_resolution("localhost") do
      {:ok, hostname} -> {:ok, hostname}
      {:error, _} -> {:error, :localhost_failed}
    end
  end

  defp try_ip_address do
    Logger.debug("Trying IP address resolution")

    case test_hostname_resolution("127.0.0.1") do
      {:ok, hostname} -> {:ok, hostname}
      {:error, reason} -> {:error, reason}
    end
  end
end
