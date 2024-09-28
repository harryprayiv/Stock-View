defmodule Budview.MixProject do
  use Mix.Project

  def project do
    [
      app: :budview,
      version: "0.1.0",
      elixir: "~> 1.14",
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
      {:phoenix, "~> 1.6.0"},
      {:phoenix_live_view, "~> 0.17.5"},
      {:phoenix_html, "~> 3.2"},
      {:jason, "~> 1.2"},
      {:postgrex, ">= 0.0.0"},
      {:ecto_sql, "~> 3.7"},
      {:phoenix_live_dashboard, "~> 0.5"},
    ]
  end
end
