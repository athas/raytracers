defmodule Raytracer.MixProject do
  use Mix.Project

  def project do
    [
      app: :raytracer,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [
        main_module: Raytracer
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.1"},
      {:parallel_stream, "~> 1.1"}
    ]
  end
end
