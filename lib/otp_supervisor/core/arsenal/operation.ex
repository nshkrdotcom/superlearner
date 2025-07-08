defmodule OTPSupervisor.Core.Arsenal.Operation do
  @moduledoc """
  Protocol for defining OTP arsenal operations that can be automatically exposed via REST API.

  This protocol enables automatic REST API generation by providing metadata about operations,
  their parameters, return types, and HTTP mapping.
  """

  @doc """
  Returns the REST endpoint configuration for this operation.

  ## Return format:
  %{
    method: :get | :post | :put | :delete | :patch,
    path: "/api/v1/path/with/:params",
    summary: "Human readable description",
    parameters: [%{name: :param_name, type: :string, required: true, description: "..."}],
    responses: %{200 => %{description: "Success", schema: %{type: :object}}}
  }
  """
  @callback rest_config() :: map()

  @doc """
  Validates input parameters for the operation.
  Returns {:ok, validated_params} or {:error, validation_errors}
  """
  @callback validate_params(params :: map()) :: {:ok, map()} | {:error, term()}

  @doc """
  Executes the operation with validated parameters.
  Returns {:ok, result} or {:error, reason}
  """
  @callback execute(params :: map()) :: {:ok, term()} | {:error, term()}

  @doc """
  Transforms the operation result for JSON response.
  """
  @callback format_response(result :: term()) :: map()

  @optional_callbacks [validate_params: 1, format_response: 1]

  defmacro __using__(_opts) do
    quote do
      @behaviour OTPSupervisor.Core.Arsenal.Operation

      def validate_params(params), do: {:ok, params}
      def format_response(result), do: %{data: result}

      defoverridable validate_params: 1, format_response: 1
    end
  end
end
