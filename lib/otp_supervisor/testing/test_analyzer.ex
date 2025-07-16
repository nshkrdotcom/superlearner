defmodule OTPSupervisor.Testing.TestAnalyzer do
  @moduledoc """
  Analyzes test files to determine distributed testing requirements.

  This module scans test files for distributed testing tags and patterns to determine:
  - Whether tests need a cluster
  - Minimum cluster size requirements
  - Test types and configurations
  - File patterns and requirement aggregation
  """

  require Logger

  @doc """
  Analyzes test files based on file patterns to determine distributed testing requirements.

  ## Parameters
  - `file_patterns` - List of file patterns or paths to analyze

  ## Returns
  - `%{needs_cluster: boolean(), min_cluster_size: integer(), test_files: list(), distributed_tests: list()}`
  """
  def analyze_test_files(file_patterns) when is_list(file_patterns) do
    file_patterns
    |> expand_file_patterns()
    |> Enum.map(&analyze_single_file/1)
    |> aggregate_requirements()
  end

  def analyze_test_files(file_pattern) when is_binary(file_pattern) do
    analyze_test_files([file_pattern])
  end

  @doc """
  Analyzes a single test file for distributed requirements.
  """
  def analyze_single_file(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        metadata = extract_test_metadata(content)
        requirements = analyze_distributed_requirements(metadata)

        %{
          file_path: file_path,
          metadata: metadata,
          requirements: requirements
        }

      {:error, reason} ->
        Logger.warning("Failed to read test file #{file_path}: #{inspect(reason)}")

        %{
          file_path: file_path,
          metadata: %{},
          requirements: %{needs_cluster: false, min_cluster_size: 0, test_type: :none},
          error: reason
        }
    end
  end

  @doc """
  Expands file patterns into actual file paths.
  """
  def expand_file_patterns(patterns) do
    patterns
    |> Enum.flat_map(&expand_single_pattern/1)
    |> Enum.filter(&File.exists?/1)
    |> Enum.filter(&String.ends_with?(&1, ".exs"))
    |> Enum.uniq()
  end

  defp expand_single_pattern(pattern) do
    cond do
      File.regular?(pattern) ->
        [pattern]

      File.dir?(pattern) ->
        Path.wildcard(Path.join(pattern, "**/*_test.exs"))

      String.contains?(pattern, "*") ->
        Path.wildcard(pattern)

      true ->
        # Try as a directory pattern
        if File.dir?(pattern) do
          Path.wildcard(Path.join(pattern, "**/*_test.exs"))
        else
          # Try as a file pattern
          Path.wildcard(pattern <> "*_test.exs")
        end
    end
  end

  @doc """
  Extracts test metadata from file content.
  """
  def extract_test_metadata(content) when is_binary(content) do
    %{
      has_distributed_tag: detect_distributed_tag(content),
      has_cluster_tag: detect_cluster_tag(content),
      has_multi_node_tag: detect_multi_node_tag(content),
      cluster_size_requirements: extract_cluster_size_tags(content),
      uses_cluster_helper: detect_cluster_helper_usage(content),
      uses_node_operations: detect_node_operations(content),
      test_module_name: extract_module_name(content),
      distributed_test_count: count_distributed_tests(content)
    }
  end

  defp detect_distributed_tag(content) do
    String.contains?(content, "@tag :distributed") or
      String.contains?(content, "@tag distributed") or
      Regex.match?(~r/@tag\s+:distributed/, content)
  end

  defp detect_cluster_tag(content) do
    String.contains?(content, "@tag :cluster") or
      String.contains?(content, "@tag cluster") or
      Regex.match?(~r/@tag\s+:cluster/, content)
  end

  defp detect_multi_node_tag(content) do
    String.contains?(content, "@tag :multi_node") or
      String.contains?(content, "@tag multi_node") or
      Regex.match?(~r/@tag\s+:multi_node/, content)
  end

  defp extract_cluster_size_tags(content) do
    # Match patterns like @tag cluster_size: 3 or @tag cluster_size: N
    cluster_size_regex = ~r/@tag\s+cluster_size:\s*(\d+)/

    cluster_size_regex
    |> Regex.scan(content, capture: :all_but_first)
    |> Enum.map(fn [size_str] -> String.to_integer(size_str) end)
  end

  defp detect_cluster_helper_usage(content) do
    String.contains?(content, "ClusterTestHelper") or
      String.contains?(content, "cluster_test_helper") or
      String.contains?(content, "start_test_cluster") or
      String.contains?(content, "stop_test_cluster")
  end

  defp detect_node_operations(content) do
    String.contains?(content, "Node.list()") or
      String.contains?(content, "Node.connect") or
      String.contains?(content, "Node.spawn") or
      String.contains?(content, ":rpc.call") or
      (String.contains?(content, "GenServer.call({") and String.contains?(content, "node}"))
  end

  defp extract_module_name(content) do
    case Regex.run(~r/defmodule\s+([A-Za-z0-9_.]+)/, content) do
      [_, module_name] -> module_name
      _ -> nil
    end
  end

  defp count_distributed_tests(content) do
    # This is a simplified count - in practice we'd need more sophisticated parsing
    # to accurately count tests with distributed tags
    distributed_tags = Regex.scan(~r/@tag\s+:distributed/, content)
    cluster_tags = Regex.scan(~r/@tag\s+:cluster/, content)
    multi_node_tags = Regex.scan(~r/@tag\s+:multi_node/, content)

    length(distributed_tags) + length(cluster_tags) + length(multi_node_tags)
  end

  @doc """
  Analyzes distributed requirements based on extracted metadata.
  """
  def analyze_distributed_requirements(metadata) do
    needs_cluster = determine_cluster_need(metadata)
    min_cluster_size = determine_min_cluster_size(metadata)
    test_type = determine_test_type(metadata)

    %{
      needs_cluster: needs_cluster,
      min_cluster_size: min_cluster_size,
      test_type: test_type,
      cluster_size_requirements: metadata[:cluster_size_requirements] || [],
      estimated_test_count: metadata[:distributed_test_count] || 0
    }
  end

  defp determine_cluster_need(metadata) do
    metadata[:has_distributed_tag] or
      metadata[:has_cluster_tag] or
      metadata[:has_multi_node_tag] or
      metadata[:uses_cluster_helper] or
      metadata[:uses_node_operations] or
      (metadata[:cluster_size_requirements] && length(metadata[:cluster_size_requirements]) > 0)
  end

  defp determine_min_cluster_size(metadata) do
    explicit_sizes = metadata[:cluster_size_requirements] || []

    cond do
      length(explicit_sizes) > 0 ->
        Enum.max(explicit_sizes)

      metadata[:has_multi_node_tag] ->
        # Multi-node typically implies at least 3 nodes
        3

      metadata[:has_distributed_tag] or metadata[:has_cluster_tag] ->
        # Basic distributed testing needs at least 2 nodes
        2

      metadata[:uses_cluster_helper] or metadata[:uses_node_operations] ->
        # Assume minimum cluster for node operations
        2

      true ->
        # No cluster needed
        0
    end
  end

  defp determine_test_type(metadata) do
    cond do
      metadata[:has_multi_node_tag] -> :multi_node
      metadata[:has_cluster_tag] -> :cluster
      metadata[:has_distributed_tag] -> :distributed
      metadata[:uses_cluster_helper] -> :cluster_helper
      metadata[:uses_node_operations] -> :node_operations
      true -> :none
    end
  end

  @doc """
  Aggregates requirements from multiple test file analyses.
  """
  def aggregate_requirements(file_analyses) do
    valid_analyses = Enum.reject(file_analyses, &Map.has_key?(&1, :error))

    needs_cluster =
      Enum.any?(valid_analyses, fn analysis ->
        analysis.requirements.needs_cluster
      end)

    min_cluster_size =
      valid_analyses
      |> Enum.map(fn analysis -> analysis.requirements.min_cluster_size end)
      |> case do
        [] -> 0
        sizes -> Enum.max(sizes)
      end

    test_files = Enum.map(valid_analyses, & &1.file_path)

    distributed_tests =
      valid_analyses
      |> Enum.filter(fn analysis -> analysis.requirements.needs_cluster end)
      |> Enum.map(fn analysis ->
        %{
          file_path: analysis.file_path,
          test_type: analysis.requirements.test_type,
          cluster_size: analysis.requirements.min_cluster_size,
          estimated_count: analysis.requirements.estimated_test_count
        }
      end)

    total_distributed_tests =
      distributed_tests
      |> Enum.map(& &1.estimated_count)
      |> Enum.sum()

    %{
      needs_cluster: needs_cluster,
      min_cluster_size: min_cluster_size,
      test_files: test_files,
      distributed_tests: distributed_tests,
      total_distributed_test_count: total_distributed_tests,
      analysis_summary: %{
        total_files_analyzed: length(file_analyses),
        files_with_errors: length(file_analyses) - length(valid_analyses),
        files_needing_cluster: length(distributed_tests),
        test_types: distributed_tests |> Enum.map(& &1.test_type) |> Enum.uniq()
      }
    }
  end

  @doc """
  Checks if the project has any distributed tests.
  """
  def has_distributed_tests? do
    # Check common test directories
    test_patterns = ["test/**/*_test.exs", "test/**/*_test.ex"]

    case analyze_test_files(test_patterns) do
      %{needs_cluster: needs_cluster} -> needs_cluster
    end
  end

  @doc """
  Checks if a specific file has distributed tests.
  """
  def file_has_distributed_tests?(file_path) do
    case analyze_single_file(file_path) do
      %{requirements: %{needs_cluster: needs_cluster}} -> needs_cluster
    end
  end

  @doc """
  Gets the required cluster size for a specific file.
  """
  def get_required_cluster_size(file_path) do
    case analyze_single_file(file_path) do
      %{requirements: %{min_cluster_size: size}} -> size
    end
  end
end
