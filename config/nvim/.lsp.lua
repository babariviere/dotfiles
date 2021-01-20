return {
  Lua = {
    runtime = {
      -- Get the language server to recognize LuaJIT globals like `jit` and `bit`
      version = "LuaJIT",
      -- Setup your lua path
      path = vim.split(package.path, ";")
    },
    diagnostics = {
      -- Get the language server to recognize the `vim` and `hs` global
      globals = {"vim"}
    },
    workspace = {
      -- Make the server aware of Neovim runtime files
      library = {[vim.fn.expand("$VIMRUNTIME/lua")] = true, [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true}
    }
  }
}
