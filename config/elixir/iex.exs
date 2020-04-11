IEx.configure(
  colors: [
    doc_title: [:yellow, :bright]
  ],
  inspect: [
    limit: 5
  ]
)

defmodule IExHelpers do
  defp expand_all(n, env) do
    Macro.prewalk(n, &Macro.expand(&1, env))
  end

  defmacro expand_rec(do: block) do
    block
    |> expand_all(__CALLER__)
    |> Macro.to_string()
    |> IO.puts()
  end

  defmacro expand(do: block) do
    block
    |> Macro.expand(__CALLER__)
    |> Macro.to_string()
    |> IO.puts()
  end
end
