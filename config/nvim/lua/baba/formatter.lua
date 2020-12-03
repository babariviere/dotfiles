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
      },
      lua = {
        function()
          return {
            exe = "lua-format",
            args = {"--config=$HOME/.config/luaformatter/config.yaml"},
            stdin = true
          }
        end
      }
    }
  })
