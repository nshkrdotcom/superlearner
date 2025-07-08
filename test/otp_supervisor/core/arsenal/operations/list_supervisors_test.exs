defmodule OTPSupervisor.Core.Arsenal.Operations.ListSupervisorsTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Core.Arsenal.Operations.ListSupervisors

  describe "rest_config/0" do
    test "returns valid REST configuration" do
      config = ListSupervisors.rest_config()

      assert config.method == :get
      assert config.path == "/api/v1/supervisors"
      assert config.summary == "List all supervisors in the system"
      assert is_list(config.parameters)
      assert is_map(config.responses)
    end

    test "has pagination parameters" do
      config = ListSupervisors.rest_config()

      param_names = Enum.map(config.parameters, & &1.name)
      assert :page in param_names
      assert :per_page in param_names
      assert :include_children in param_names
      assert :filter_application in param_names
    end
  end

  describe "validate_params/1" do
    test "validates default parameters" do
      assert {:ok, validated} = ListSupervisors.validate_params(%{})

      assert validated["include_children"] == false
      assert validated["filter_application"] == nil
      assert validated["page"] == 1
      assert validated["per_page"] == 50
    end

    test "validates custom parameters" do
      params = %{
        "include_children" => true,
        "filter_application" => "my_app",
        "page" => 2,
        "per_page" => 25
      }

      assert {:ok, validated} = ListSupervisors.validate_params(params)

      assert validated["include_children"] == true
      assert validated["filter_application"] == "my_app"
      assert validated["page"] == 2
      assert validated["per_page"] == 25
    end

    test "validates string pagination parameters" do
      params = %{"page" => "3", "per_page" => "20"}

      assert {:ok, validated} = ListSupervisors.validate_params(params)

      assert validated["page"] == 3
      assert validated["per_page"] == 20
    end

    test "caps per_page at maximum" do
      params = %{"per_page" => "200"}

      assert {:ok, validated} = ListSupervisors.validate_params(params)

      # Should be capped at 100
      assert validated["per_page"] == 100
    end

    test "handles invalid pagination gracefully" do
      params = %{"page" => "invalid", "per_page" => "-5"}

      assert {:ok, validated} = ListSupervisors.validate_params(params)

      # Falls back to default
      assert validated["page"] == 1
      # Falls back to default
      assert validated["per_page"] == 50
    end

    test "validates boolean parameters" do
      test_cases = [
        {true, true},
        {false, false},
        # String values converted to boolean
        {"true", true},
        {"false", false}
      ]

      Enum.each(test_cases, fn {input, expected} ->
        params = %{"include_children" => input}

        assert {:ok, validated} = ListSupervisors.validate_params(params)
        assert validated["include_children"] == expected
      end)
    end
  end

  describe "execute/1" do
    test "discovers system supervisors" do
      params = %{
        "include_children" => false,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 50
      }

      assert {:ok, {supervisors, meta}} = ListSupervisors.execute(params)

      assert is_list(supervisors)
      assert is_map(meta)
      assert Map.has_key?(meta, :total)
      assert Map.has_key?(meta, :page)
      assert Map.has_key?(meta, :per_page)
      assert Map.has_key?(meta, :total_pages)
    end

    test "includes children information when requested" do
      params = %{
        "include_children" => true,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 10
      }

      assert {:ok, {supervisors, _meta}} = ListSupervisors.execute(params)

      # At least some supervisors should have children info
      if length(supervisors) > 0 do
        supervisor = List.first(supervisors)
        assert Map.has_key?(supervisor, :child_count)
        assert Map.has_key?(supervisor, :strategy)
        assert Map.has_key?(supervisor, :children)
      end
    end

    test "filters by application when specified" do
      # First get all supervisors to find a valid application
      all_params = %{
        "include_children" => false,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 100
      }

      assert {:ok, {all_supervisors, _}} = ListSupervisors.execute(all_params)

      if length(all_supervisors) > 0 do
        # Find a supervisor with an application
        supervisor_with_app =
          Enum.find(all_supervisors, fn sup ->
            sup.application != :system
          end)

        if supervisor_with_app do
          app_name = Atom.to_string(supervisor_with_app.application)

          filtered_params = %{
            "include_children" => false,
            "filter_application" => app_name,
            "page" => 1,
            "per_page" => 50
          }

          assert {:ok, {filtered_supervisors, _}} = ListSupervisors.execute(filtered_params)

          # All returned supervisors should belong to the specified application
          target_app = String.to_existing_atom(app_name)

          for supervisor <- filtered_supervisors do
            assert supervisor.application == target_app
          end
        end
      end
    end

    test "handles pagination correctly" do
      # Get total count first
      params_page1 = %{
        "include_children" => false,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 2
      }

      assert {:ok, {supervisors_page1, meta1}} = ListSupervisors.execute(params_page1)

      if meta1.total > 2 do
        # Get second page
        params_page2 = %{
          "include_children" => false,
          "filter_application" => nil,
          "page" => 2,
          "per_page" => 2
        }

        assert {:ok, {supervisors_page2, meta2}} = ListSupervisors.execute(params_page2)

        # Different supervisors on different pages
        page1_names = Enum.map(supervisors_page1, & &1.name)
        page2_names = Enum.map(supervisors_page2, & &1.name)

        assert page1_names != page2_names
        assert meta1.page == 1
        assert meta2.page == 2
        assert meta1.total == meta2.total
      end
    end

    test "returns empty list when no supervisors match filter" do
      params = %{
        "include_children" => false,
        "filter_application" => "non_existent_app",
        "page" => 1,
        "per_page" => 50
      }

      assert {:ok, {supervisors, meta}} = ListSupervisors.execute(params)

      assert supervisors == []
      assert meta.total == 0
    end

    test "handles large page numbers gracefully" do
      params = %{
        "include_children" => false,
        "filter_application" => nil,
        "page" => 999,
        "per_page" => 50
      }

      assert {:ok, {supervisors, meta}} = ListSupervisors.execute(params)

      assert is_list(supervisors)
      assert meta.page == 999
      # Should return empty list if page is beyond available data
    end
  end

  describe "format_response/1" do
    test "formats supervisor list correctly" do
      # Create mock supervisor data
      supervisors = [
        %{
          name: :test_supervisor,
          pid: self(),
          alive: true,
          child_count: 3,
          strategy: :one_for_one,
          application: :test_app,
          children: []
        }
      ]

      meta = %{
        total: 1,
        page: 1,
        per_page: 50,
        total_pages: 1
      }

      formatted = ListSupervisors.format_response({supervisors, meta})

      assert %{data: data, meta: response_meta} = formatted
      assert is_list(data)
      assert length(data) == 1

      supervisor = List.first(data)
      assert supervisor.name == "test_supervisor"
      assert is_binary(supervisor.pid)
      assert supervisor.alive == true
      assert supervisor.child_count == 3
      assert supervisor.strategy == :one_for_one
      assert supervisor.application == :test_app

      assert response_meta == meta
    end

    test "formats supervisor names correctly" do
      test_cases = [
        {:atom_name, "atom_name"},
        {nil, "nil"}
      ]

      Enum.each(test_cases, fn {input_name, expected_name} ->
        supervisors = [
          %{
            name: input_name,
            pid: self(),
            alive: true,
            application: :test_app
          }
        ]

        meta = %{total: 1, page: 1, per_page: 50, total_pages: 1}
        formatted = ListSupervisors.format_response({supervisors, meta})

        supervisor = formatted.data |> List.first()
        assert supervisor.name == expected_name
      end)
    end

    test "formats PIDs as strings" do
      test_pid = spawn(fn -> :ok end)

      supervisors = [
        %{
          name: :test_sup,
          pid: test_pid,
          alive: true,
          application: :test_app
        }
      ]

      meta = %{total: 1, page: 1, per_page: 50, total_pages: 1}
      formatted = ListSupervisors.format_response({supervisors, meta})

      supervisor = formatted.data |> List.first()
      assert is_binary(supervisor.pid)
      assert supervisor.pid == inspect(test_pid)
    end

    test "includes children information when present" do
      children = [
        %{id: :child1, pid: inspect(self()), type: :worker, modules: [TestWorker]},
        %{id: :child2, pid: :undefined, type: :supervisor, modules: [TestSupervisor]}
      ]

      supervisors = [
        %{
          name: :test_sup,
          pid: self(),
          alive: true,
          child_count: 2,
          strategy: :one_for_one,
          application: :test_app,
          children: children
        }
      ]

      meta = %{total: 1, page: 1, per_page: 50, total_pages: 1}
      formatted = ListSupervisors.format_response({supervisors, meta})

      supervisor = formatted.data |> List.first()
      assert length(supervisor.children) == 2
      assert is_list(supervisor.children)

      child = List.first(supervisor.children)
      assert Map.has_key?(child, :id)
      assert Map.has_key?(child, :pid)
      assert Map.has_key?(child, :type)
      assert Map.has_key?(child, :modules)
    end
  end

  describe "supervisor discovery" do
    setup do
      # Start a test supervisor for discovery
      {:ok, test_sup} = Task.Supervisor.start_link(name: :test_discovery_supervisor)

      on_exit(fn ->
        if Process.alive?(test_sup) do
          try do
            GenServer.stop(test_sup)
          rescue
            _ -> :ok
          catch
            :exit, _ -> :ok
          end
        end
      end)

      %{test_supervisor: test_sup}
    end

    test "discovers test supervisor", %{test_supervisor: test_sup} do
      params = %{
        "include_children" => false,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 100
      }

      assert {:ok, {supervisors, _meta}} = ListSupervisors.execute(params)

      # Should find our test supervisor
      test_sup_found =
        Enum.any?(supervisors, fn sup ->
          sup.name == :test_discovery_supervisor or sup.pid == test_sup
        end)

      assert test_sup_found
    end

    test "gets children information for test supervisor", %{test_supervisor: test_sup} do
      params = %{
        "include_children" => true,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 100
      }

      assert {:ok, {supervisors, _meta}} = ListSupervisors.execute(params)

      # Find our test supervisor
      test_supervisor =
        Enum.find(supervisors, fn sup ->
          sup.name == :test_discovery_supervisor or sup.pid == test_sup
        end)

      if test_supervisor do
        assert is_integer(test_supervisor.child_count)
        assert is_atom(test_supervisor.strategy)
        assert is_list(test_supervisor.children)
      end
    end
  end

  describe "integration tests" do
    test "full operation flow works" do
      # Full validation -> execution -> formatting flow
      raw_params = %{
        "include_children" => "true",
        "page" => "1",
        "per_page" => "10"
      }

      assert {:ok, validated_params} = ListSupervisors.validate_params(raw_params)
      assert {:ok, result} = ListSupervisors.execute(validated_params)
      assert %{data: _supervisors, meta: _meta} = ListSupervisors.format_response(result)
    end

    test "handles empty system gracefully" do
      # This test depends on system state, but should handle gracefully
      params = %{
        "include_children" => false,
        "filter_application" => "definitely_not_an_app",
        "page" => 1,
        "per_page" => 50
      }

      assert {:ok, {supervisors, meta}} = ListSupervisors.execute(params)
      assert is_list(supervisors)
      assert is_map(meta)
      assert meta.total >= 0
    end
  end

  describe "performance tests" do
    test "handles large page sizes efficiently" do
      start_time = System.monotonic_time(:millisecond)

      params = %{
        "include_children" => true,
        "filter_application" => nil,
        "page" => 1,
        "per_page" => 100
      }

      assert {:ok, {_supervisors, _meta}} = ListSupervisors.execute(params)

      end_time = System.monotonic_time(:millisecond)
      execution_time = end_time - start_time

      # Should complete within reasonable time (adjust based on system)
      # 5 seconds max
      assert execution_time < 5000
    end
  end
end
