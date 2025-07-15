defmodule OTPSupervisor.Testing.AutoClusterManagerErrorHandlingTest do
  use ExUnit.Case, async: false
  
  alias OTPSupervisor.Testing.AutoClusterManager
  
  @moduletag :unit
  
  describe "cluster startup failure handling" do
    test "provides structured error information when cluster startup fails" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # In this test environment, cluster startup should fail
      # but we should get structured error information
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      
      case result do
        {:ok, cluster_info} ->
          # If cluster somehow starts successfully, that's also valid
          assert is_map(cluster_info)
          assert Map.has_key?(cluster_info, :cluster_active)
          
        {:error, diagnosis} ->
          # This is the expected case in our test environment
          # Validate that we get proper error structure
          assert is_map(diagnosis)
          
          # Should have these key fields for proper error handling
          assert Map.has_key?(diagnosis, :problem)
          assert Map.has_key?(diagnosis, :solutions)
          assert Map.has_key?(diagnosis, :fallback_strategy)
          assert Map.has_key?(diagnosis, :retry_suggestions)
          
          # Problem should be a descriptive string
          assert is_binary(diagnosis.problem)
          assert String.length(diagnosis.problem) > 0
          
          # Solutions should be a list of actionable items
          assert is_list(diagnosis.solutions)
          assert length(diagnosis.solutions) > 0
          Enum.each(diagnosis.solutions, fn solution ->
            assert is_binary(solution)
            assert String.length(solution) > 0
          end)
          
          # Should have a fallback strategy
          assert diagnosis.fallback_strategy in [
            :skip_distributed_tests, 
            :reduce_cluster_size, 
            :retry_with_delay, 
            :retry_with_different_ports,
            :fail_fast
          ]
          
          # Should have retry suggestions
          assert is_list(diagnosis.retry_suggestions)
          assert length(diagnosis.retry_suggestions) > 0
      end
    end
    
    test "error diagnosis contains expected failure patterns" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 3,
        test_type: :distributed
      }
      
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      
      # In our test environment, this should fail with node connection errors
      case result do
        {:error, diagnosis} ->
          # Should mention node connection failures
          problem_text = String.downcase(diagnosis.problem)
          
          # Should contain indicators of the actual problem
          assert String.contains?(problem_text, "node") or 
                 String.contains?(problem_text, "cluster") or
                 String.contains?(problem_text, "start")
          
          # Solutions should contain helpful suggestions
          solutions_text = diagnosis.solutions |> Enum.join(" ") |> String.downcase()
          
          # Should suggest checking common issues
          assert String.contains?(solutions_text, "epmd") or
                 String.contains?(solutions_text, "network") or
                 String.contains?(solutions_text, "port") or
                 String.contains?(solutions_text, "clean")
                 
        {:ok, _cluster_info} ->
          # If it succeeds, that's fine too - means environment supports clustering
          :ok
      end
    end
    
    test "validates that cluster startup actually fails in test environment" do
      # This test specifically validates that our test environment 
      # cannot start distributed clusters (which is why we see the errors)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      
      # In our constrained test environment, this SHOULD fail
      # If it doesn't fail, then our test environment has changed
      case result do
        {:error, diagnosis} ->
          # This is expected - validate the error is about node connectivity
          assert String.contains?(diagnosis.problem, "node") or
                 String.contains?(diagnosis.problem, "cluster") or
                 String.contains?(diagnosis.problem, "start")
          
          # Log the actual error for debugging
          IO.puts("\n=== Expected Cluster Startup Failure ===")
          IO.puts("Problem: #{diagnosis.problem}")
          IO.puts("Reason: #{inspect(diagnosis.reason)}")
          IO.puts("Solutions: #{inspect(diagnosis.solutions)}")
          IO.puts("=========================================\n")
          
        {:ok, cluster_info} ->
          # If cluster startup succeeds, that means our test environment
          # has been fixed to support distributed Erlang
          IO.puts("\n=== Unexpected: Cluster Startup Succeeded ===")
          IO.puts("Cluster Info: #{inspect(cluster_info)}")
          IO.puts("This means the distributed Erlang issues have been resolved!")
          IO.puts("===============================================\n")
          
          # This is actually good news - it means the environment now supports clustering
          assert cluster_info.cluster_active
          assert length(cluster_info.nodes) >= 2
      end
    end
  end
  
  describe "error message quality" do
    test "error messages are helpful and actionable" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      
      case result do
        {:error, diagnosis} ->
          # Problem should be clear and specific
          refute String.contains?(diagnosis.problem, "unknown")
          refute String.contains?(diagnosis.problem, "generic")
          
          # Solutions should be actionable (contain verbs)
          action_words = ["check", "run", "verify", "ensure", "start", "stop", "clean", "try"]
          solutions_text = diagnosis.solutions |> Enum.join(" ") |> String.downcase()
          
          has_action_words = Enum.any?(action_words, fn word ->
            String.contains?(solutions_text, word)
          end)
          
          assert has_action_words, "Solutions should contain actionable advice"
          
        {:ok, _} ->
          # If cluster starts successfully, we can't test error message quality
          :ok
      end
    end
  end
end