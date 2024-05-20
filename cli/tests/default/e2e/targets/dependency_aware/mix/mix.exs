defmodule Myapp.MixProject do
  use Mix.Project

  def project do
    [
      app: :myapp,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_aws_s3, "~> 2.0"},
      {:ex_aws_lambda, "~> 2.0"},
      {:ex_aws_ses, "~> 2.0"},
      {:hackney, "~> 1.17"},
      {:jason, "~> 1.0"},
      # some comment
      {:joken, "~> 2.0"},
      {:sweet_xml, "~> 0.7.1"},
      {:uuid, "~> 1.1"}
    ]
  end
end
