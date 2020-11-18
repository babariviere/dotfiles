require('format').setup({
    elixir = {
      mix = function()
        return {
          exe = "mix",
          args = {"format", "-"},
          stdin = true
        }
      end
    }
  })
