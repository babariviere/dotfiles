require('formatter').setup({
    filetype = {
      elixir = {
        function()
          return {
            exe = "mix",
            args = {"format", "-"},
            stdin = true
          }
        end
      }
    }
  })
