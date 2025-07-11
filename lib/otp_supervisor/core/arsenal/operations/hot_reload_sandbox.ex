defmodule OTPSupervisor.Core.Arsenal.Operations.HotReloadSandbox do
  @moduledoc """
  Operation to hot reload a sandbox with updated code.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/sandboxes/:sandbox_id/hot-reload",
      summary: "Hot reload a sandbox with updated code",
      parameters: [
        %{
          name: :sandbox_id,
          type: :string,
          required: true,
          description: "Unique identifier of the sandbox to reload",
          location: :path
        },
        %{
          name: :module,
          type: :string,
          required: false,
          description: "Specific module to reload (defaults to supervisor module)",
          location: :body
        }
      ],
      responses: %{
        200 => %{
          description: "Hot reload successful",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  sandbox_id: %{type: :string},
                  reloaded_module: %{type: :string},
                  previous_version: %{type: :integer},
                  new_version: %{type: :integer},
                  compilation_time_ms: %{type: :integer},
                  status: %{type: :string}
                }
              }
            }
          }
        },
        404 => %{description: "Sandbox not found"},
        400 => %{description: "Invalid parameters"},
        500 => %{description: "Hot reload failed"}
      }
    }
  end

  def validate_params(%{"sandbox_id" => sandbox_id} = params) do
    validated_params = %{
      "sandbox_id" => validate_sandbox_id(sandbox_id),
      "module" => Map.get(params, "module")
    }

    {:ok, validated_params}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, "sandbox_id is required"}}
  end

  @dialyzer {:nowarn_function, execute: 1}
  def execute(%{"sandbox_id" => sandbox_id, "module" => module_name}) do
    case OTPSupervisor.Core.SandboxManager.get_sandbox_info(sandbox_id) do
      {:ok, sandbox_info} ->
        # Determine which module to reload
        target_module =
          case module_name do
            nil -> sandbox_info.supervisor_module
            name -> String.to_existing_atom("Elixir." <> name)
          end

        # Get current version
        current_version =
          case OTPSupervisor.Core.ModuleVersionManager.get_current_version(
                 sandbox_id,
                 target_module
               ) do
            {:ok, version} -> version
            {:error, :not_found} -> 0
          end

        # Compile updated code
        compile_start = System.monotonic_time(:millisecond)

        # Determine sandbox path based on sandbox info
        sandbox_path = determine_sandbox_path(sandbox_info)

        case OTPSupervisor.Core.IsolatedCompiler.compile_sandbox(sandbox_path) do
          {:ok, compile_info} ->
            compile_time = System.monotonic_time(:millisecond) - compile_start

            # Find the target module's BEAM file
            module_beam = find_module_beam_file(compile_info.beam_files, target_module)

            case module_beam do
              nil ->
                {:error, {:module_not_found, target_module}}

              beam_file ->
                case File.read(beam_file) do
                  {:ok, beam_data} ->
                    # Perform hot swap
                    case OTPSupervisor.Core.ModuleVersionManager.hot_swap_module(
                           sandbox_id,
                           target_module,
                           beam_data
                         ) do
                      {:ok, :hot_swapped} ->
                        {:ok, new_version} =
                          OTPSupervisor.Core.ModuleVersionManager.get_current_version(
                            sandbox_id,
                            target_module
                          )

                        {:ok,
                         %{
                           sandbox_id: sandbox_id,
                           reloaded_module: Atom.to_string(target_module),
                           previous_version: current_version,
                           new_version: new_version,
                           compilation_time_ms: compile_time,
                           status: "hot_swapped"
                         }}

                      {:ok, :no_change} ->
                        {:ok,
                         %{
                           sandbox_id: sandbox_id,
                           reloaded_module: Atom.to_string(target_module),
                           previous_version: current_version,
                           new_version: current_version,
                           compilation_time_ms: compile_time,
                           status: "no_change"
                         }}

                      {:error, reason} ->
                        {:error, {:hot_swap_failed, reason}}
                    end

                  {:error, reason} ->
                    {:error, {:beam_read_failed, reason}}
                end
            end

          {:error, reason} ->
            {:error, {:compilation_failed, reason}}
        end

      {:error, :not_found} ->
        {:error, :sandbox_not_found}

      {:error, reason} ->
        {:error, {:sandbox_info_failed, reason}}
    end
  end

  def format_response(result) do
    %{data: result}
  end

  defp validate_sandbox_id(sandbox_id) when is_binary(sandbox_id) and byte_size(sandbox_id) > 0 do
    sandbox_id
  end

  defp validate_sandbox_id(_), do: raise("sandbox_id must be a non-empty string")

  defp determine_sandbox_path(sandbox_info) do
    # Use the same path construction logic as SandboxManager
    app_name = sandbox_info.app_name
    Path.join([File.cwd!(), "sandbox", "examples", to_string(app_name)])
  end

  @dialyzer {:nowarn_function, find_module_beam_file: 2}
  defp find_module_beam_file(beam_files, target_module) do
    module_name = target_module |> Atom.to_string() |> String.replace("Elixir.", "")

    Enum.find(beam_files, fn beam_file ->
      String.contains?(beam_file, "#{module_name}.beam")
    end)
  end
end
