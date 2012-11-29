defmodule Ccl5.Mixfile do
  use Mix.Project

  def project do
    [ app: :ccl5,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:ibrowse, :hackney, :crypto, :xmerl],
      env: [
            account: 'dimimd',
            azure_key: 'V4slYOCV2cp8n2oKKbRnYzhCnchVoheBnB3cbJK35Na/Fmf+1hZ/UNoCVGLPbwCs7phi6F9gaap6v14zOONalg=='
           ]]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
