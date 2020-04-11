IEx.configure(
  colors: [
    doc_title: [:yellow, :bright]
  ],
  inspect: [
    limit: 5
  ]
)

defmodule Helpers do
  def expand(macro) do
    macro
    |> Macro.expand(__ENV__)
    |> Macro.to_string()
    |> IO.puts()
  end
end

import Helpers
