defmodule OTPSupervisor.Testing.TestAnalyzerTest do
  use ExUnit.Case, async: true
  
  alias OTPSupervisor.Testing.TestAnalyzer
  
  describe "extract_test_metadata/1" do
    test "detects distributed tag" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        @tag :distributed
        test "distributed functionality" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      assert metadata.has_distributed_tag
      refute metadata.has_cluster_tag
      refute metadata.has_multi_node_tag
    end
    
    test "detects cluster tag" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        @tag :cluster
        test "cluster functionality" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      refute metadata.has_distributed_tag
      assert metadata.has_cluster_tag
      refute metadata.has_multi_node_tag
    end
    
    test "detects multi_node tag" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        @tag :multi_node
        test "multi node functionality" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      refute metadata.has_distributed_tag
      refute metadata.has_cluster_tag
      assert metadata.has_multi_node_tag
    end
    
    test "extracts cluster size requirements" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        @tag cluster_size: 3
        test "three node test" do
          assert true
        end
        
        @tag cluster_size: 5
        test "five node test" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      assert metadata.cluster_size_requirements == [3, 5]
    end
    
    test "detects cluster helper usage" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        setup do
          {:ok, nodes} = ClusterTestHelper.start_test_cluster(2)
          {:ok, nodes: nodes}
        end
        
        test "uses cluster helper" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      assert metadata.uses_cluster_helper
    end
    
    test "detects node operations" do
      content = """
      defmodule MyTest do
        use ExUnit.Case
        
        test "node operations" do
          nodes = Node.list()
          Node.connect(:other_node)
          assert length(nodes) > 0
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      assert metadata.uses_node_operations
    end
    
    test "extracts module name" do
      content = """
      defmodule MyApp.DistributedTest do
        use ExUnit.Case
        
        test "something" do
          assert true
        end
      end
      """
      
      metadata = TestAnalyzer.extract_test_metadata(content)
      
      assert metadata.test_module_name == "MyApp.DistributedTest"
    end
  end
  
  describe "analyze_distributed_requirements/1" do
    test "determines cluster need based on distributed tag" do
      metadata = %{
        has_distributed_tag: true,
        has_cluster_tag: false,
        has_multi_node_tag: false,
        cluster_size_requirements: [],
        uses_cluster_helper: false,
        uses_node_operations: false
      }
      
      requirements = TestAnalyzer.analyze_distributed_requirements(metadata)
      
      assert requirements.needs_cluster
      assert requirements.min_cluster_size == 2
      assert requirements.test_type == :distributed
    end
    
    test "determines cluster need based on cluster helper usage" do
      metadata = %{
        has_distributed_tag: false,
        has_cluster_tag: false,
        has_multi_node_tag: false,
        cluster_size_requirements: [],
        uses_cluster_helper: true,
        uses_node_operations: false
      }
      
      requirements = TestAnalyzer.analyze_distributed_requirements(metadata)
      
      assert requirements.needs_cluster
      assert requirements.min_cluster_size == 2
      assert requirements.test_type == :cluster_helper
    end
    
    test "determines minimum cluster size from explicit requirements" do
      metadata = %{
        has_distributed_tag: true,
        has_cluster_tag: false,
        has_multi_node_tag: false,
        cluster_size_requirements: [3, 5, 2],
        uses_cluster_helper: false,
        uses_node_operations: false
      }
      
      requirements = TestAnalyzer.analyze_distributed_requirements(metadata)
      
      assert requirements.needs_cluster
      assert requirements.min_cluster_size == 5
      assert requirements.test_type == :distributed
    end
    
    test "sets multi_node minimum to 3" do
      metadata = %{
        has_distributed_tag: false,
        has_cluster_tag: false,
        has_multi_node_tag: true,
        cluster_size_requirements: [],
        uses_cluster_helper: false,
        uses_node_operations: false
      }
      
      requirements = TestAnalyzer.analyze_distributed_requirements(metadata)
      
      assert requirements.needs_cluster
      assert requirements.min_cluster_size == 3
      assert requirements.test_type == :multi_node
    end
    
    test "no cluster needed for regular tests" do
      metadata = %{
        has_distributed_tag: false,
        has_cluster_tag: false,
        has_multi_node_tag: false,
        cluster_size_requirements: [],
        uses_cluster_helper: false,
        uses_node_operations: false
      }
      
      requirements = TestAnalyzer.analyze_distributed_requirements(metadata)
      
      refute requirements.needs_cluster
      assert requirements.min_cluster_size == 0
      assert requirements.test_type == :none
    end
  end
  
  describe "aggregate_requirements/1" do
    test "aggregates multiple file analyses" do
      file_analyses = [
        %{
          file_path: "test/file1_test.exs",
          requirements: %{
            needs_cluster: true,
            min_cluster_size: 2,
            test_type: :distributed,
            estimated_test_count: 3
          }
        },
        %{
          file_path: "test/file2_test.exs",
          requirements: %{
            needs_cluster: true,
            min_cluster_size: 5,
            test_type: :multi_node,
            estimated_test_count: 2
          }
        },
        %{
          file_path: "test/file3_test.exs",
          requirements: %{
            needs_cluster: false,
            min_cluster_size: 0,
            test_type: :none,
            estimated_test_count: 0
          }
        }
      ]
      
      result = TestAnalyzer.aggregate_requirements(file_analyses)
      
      assert result.needs_cluster
      assert result.min_cluster_size == 5
      assert length(result.test_files) == 3
      assert length(result.distributed_tests) == 2
      assert result.total_distributed_test_count == 5
      
      assert result.analysis_summary.total_files_analyzed == 3
      assert result.analysis_summary.files_needing_cluster == 2
      assert :distributed in result.analysis_summary.test_types
      assert :multi_node in result.analysis_summary.test_types
    end
    
    test "handles files with errors" do
      file_analyses = [
        %{
          file_path: "test/good_test.exs",
          requirements: %{
            needs_cluster: true,
            min_cluster_size: 2,
            test_type: :distributed,
            estimated_test_count: 1
          }
        },
        %{
          file_path: "test/bad_test.exs",
          error: :enoent
        }
      ]
      
      result = TestAnalyzer.aggregate_requirements(file_analyses)
      
      assert result.needs_cluster
      assert result.min_cluster_size == 2
      assert length(result.test_files) == 1
      assert result.analysis_summary.files_with_errors == 1
    end
  end
  
  describe "expand_file_patterns/1" do
    test "handles single file pattern" do
      # Create a temporary test file
      test_file = "test_temp_file_test.exs"
      File.write!(test_file, "# test content")
      
      on_exit(fn -> File.rm(test_file) end)
      
      result = TestAnalyzer.expand_file_patterns([test_file])
      
      assert test_file in result
    end
    
    test "filters non-existent files" do
      result = TestAnalyzer.expand_file_patterns(["non_existent_test.exs"])
      
      assert result == []
    end
    
    test "filters non-test files" do
      # Create a temporary non-test file
      non_test_file = "temp_file.ex"
      File.write!(non_test_file, "# not a test")
      
      on_exit(fn -> File.rm(non_test_file) end)
      
      result = TestAnalyzer.expand_file_patterns([non_test_file])
      
      assert result == []
    end
  end
end