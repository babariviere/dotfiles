require('formatter').setup({
  filetype = {
    css = {
      function()
        return {
          exe = 'prettier',
          args = {
            '--stdin-filepath', vim.api.nvim_buf_get_name(0), '--single-quote'
          },
          stdin = true
        }
      end
    },
    elixir = {
      function() return {exe = 'mix', args = {'format', '-'}, stdin = true} end
    },
    lua = {
      function()
        return {
          exe = 'lua-format',
          args = {'--config=$HOME/.config/luaformatter/config.yaml'},
          stdin = true
        }
      end
    }
  }
})
