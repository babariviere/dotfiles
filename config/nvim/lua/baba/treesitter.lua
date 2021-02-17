require "nvim-treesitter.configs".setup {
  ensure_installed = "maintained", -- one of "all", "language", or a list of languages
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = {}
    -- disable = { "c", "rust" },  -- list of language that will be disabled
  },
  incremental_selection = {
    enable = true,
    keymaps = {init_selection = "gnn", node_incremental = "grn", scope_incremental = "grc", node_decremental = "grm"}
  },
  indent = {enable = true},
  query_linter = {enable = true, use_virtual_text = true, lint_events = {"BufWrite", "CursorHold"}},
  rainbow = {
    enable = true
  },
  refactor = {
    highlight_definitions = {enable = false},
    highlight_current_scope = {enable = false},
    smart_rename = {enable = true, keymaps = {smart_rename = "grr"}},
    navigation = {
      enable = true,
      keymaps = {
        goto_definition_lsp_fallback = "gd",
        list_definitions = "gD",
        list_definitions_toc = "gO",
        goto_next_usage = "<a-*>",
        goto_previous_usage = "<a-#>"
      }
    }
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner"
      }
    },
    swap = {
      enable = true,
      swap_next = {["<leader>a"] = "@parameter.inner"},
      swap_previous = {["<leader>A"] = "@parameter.inner"}
    },
    move = {
      enable = true,
      goto_next_start = {["]m"] = "@function.outer", ["]]"] = "@class.outer"},
      goto_next_end = {["]M"] = "@function.outer", ["]["] = "@class.outer"},
      goto_previous_start = {["[m"] = "@function.outer", ["[["] = "@class.outer"},
      goto_previous_end = {["[M"] = "@function.outer", ["[]"] = "@class.outer"}
    },
    lsp_interop = {enable = true, peek_definition_code = {["df"] = "@function.outer", ["dF"] = "@class.outer"}}
  },
  tree_docs = {
    enable = true,
    keymaps = {doc_all_in_range = "gdd", doc_node_at_cursor = "gdd", edit_doc_at_cursor = "gde"}
  }
}
