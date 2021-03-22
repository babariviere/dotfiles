local gl = require("galaxyline")
local gls = gl.section
gl.short_line_list = {"LuaTree", "vista", "dbui"}

local colors = {
  yellow = "#fabd2f",
  cyan = "#008080",
  green = "#afd700",
  orange = "#FF8800",
  purple = "#5d4d7a",
  magenta = "#d16d9e",
  grey = "#c0c0c0",
  blue = "#0087d7",
  red = "#ec5f67"
}

local buffer_not_empty = function()
  if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
    return true
  end
  return false
end

gls.left[1] = {
  FirstElement = {
    provider = function()
      return "▊ "
    end,
    highlight = {colors.blue, colors.line_bg}
  }
}
gls.left[2] = {
  ViMode = {
    provider = function()
      -- auto change color according the vim mode
      local mode_color = {
        n = colors.magenta,
        i = colors.green,
        v = colors.blue,
        [""] = colors.blue,
        V = colors.blue,
        c = colors.red,
        no = colors.magenta,
        s = colors.orange,
        S = colors.orange,
        [""] = colors.orange,
        ic = colors.yellow,
        R = colors.purple,
        Rv = colors.purple,
        cv = colors.red,
        ce = colors.red,
        r = colors.cyan,
        rm = colors.cyan,
        ["r?"] = colors.cyan,
        ["!"] = colors.red,
        t = colors.red
      }
      vim.api.nvim_command("hi GalaxyViMode guifg=" .. mode_color[vim.fn.mode()])
      return "  "
    end,
    highlight = {colors.red, colors.line_bg, "bold"}
  }
}
gls.left[3] = {
  FileIcon = {
    provider = "FileIcon",
    condition = buffer_not_empty,
    highlight = {require("galaxyline.provider_fileinfo").get_file_icon_color, colors.line_bg}
  }
}
gls.left[4] = {
  FileName = {
    provider = {"FileName", "FileSize"},
    condition = buffer_not_empty,
    highlight = {colors.fg, colors.line_bg, "bold"}
  }
}

local function find_git_root()
  local path = vim.fn.expand("%:p:h")
  local get_git_dir = require("galaxyline.provider_vcs").get_git_dir
  return get_git_dir(path)
end

gls.left[5] = {
  GitIcon = {
    provider = function()
      return "  "
    end,
    condition = find_git_root,
    highlight = {colors.orange, colors.line_bg}
  }
}
gls.left[6] = {
  GitBranch = {provider = "GitBranch", condition = find_git_root, highlight = {colors.fg, colors.line_bg, "bold"}}
}

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 40 then
    return true
  end
  return false
end

gls.left[7] = {
  DiffAdd = {provider = "DiffAdd", condition = checkwidth, icon = " ", highlight = {colors.green, colors.line_bg}}
}
gls.left[8] = {
  DiffModified = {
    provider = "DiffModified",
    condition = checkwidth,
    icon = " ",
    highlight = {colors.orange, colors.line_bg}
  }
}
gls.left[9] = {
  DiffRemove = {
    provider = "DiffRemove",
    condition = checkwidth,
    icon = " ",
    highlight = {colors.red, colors.line_bg}
  }
}
gls.left[10] = {
  LeftEnd = {
    provider = function()
      return ""
    end,
    separator = "",
    separator_highlight = {colors.bg, colors.line_bg},
    highlight = {colors.line_bg, colors.line_bg}
  }
}
gls.left[11] = {DiagnosticError = {provider = "DiagnosticError", icon = "  ", highlight = {colors.red, colors.bg}}}
gls.left[12] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = {colors.bg, colors.bg}
  }
}
gls.left[13] = {DiagnosticWarn = {provider = "DiagnosticWarn", icon = "  ", highlight = {colors.blue, colors.bg}}}
gls.left[14] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = {colors.bg, colors.bg}
  }
}

local messages = require("lsp-status/messaging").messages

local lsp_config = {
  kind_labels = {},
  indicator_errors = "",
  indicator_warnings = "",
  indicator_info = "",
  indicator_hint = "",
  indicator_ok = "",
  spinner_frames = {"⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"},
  status_symbol = "異",
  select_symbol = nil
}
local function statusline_lsp()
  if #vim.lsp.buf_get_clients() == 0 then
    return ""
  end

  local buf_messages = messages()
  local only_hint = true
  local status_parts = {}

  local msgs = {}
  for _, msg in ipairs(buf_messages) do
    local name = msg.name
    local client_name = "[" .. name .. "]"
    local contents = ""
    if msg.progress then
      contents = msg.title
      if msg.message then
        contents = contents .. " " .. msg.message
      end

      if msg.percentage then
        contents = contents .. " (" .. msg.percentage .. ")"
      end

      if msg.spinner then
        contents = lsp_config.spinner_frames[(msg.spinner % #lsp_config.spinner_frames) + 1] .. " " .. contents
      end
    elseif msg.status then
      contents = msg.content
      if msg.uri then
        local filename = vim.uri_to_fname(msg.uri)
        filename = vim.fn.fnamemodify(filename, ":~:.")
        local space = math.min(60, math.floor(0.6 * vim.fn.winwidth(0)))
        if #filename > space then
          filename = vim.fn.pathshorten(filename)
        end

        contents = "(" .. filename .. ") " .. contents
      end
    else
      contents = msg.content
    end

    table.insert(msgs, client_name .. " " .. contents)
  end

  local base_status = vim.trim(table.concat(status_parts, " ") .. " " .. table.concat(msgs, " "))
  local symbol = lsp_config.status_symbol .. (only_hint and "" or " ")
  local current_function = vim.b.lsp_current_function
  if current_function and current_function ~= "" then
    symbol = symbol .. "(" .. current_function .. ") "
  end

  if base_status ~= "" then
    return symbol .. base_status
  end

  return symbol .. (vim.b.lsp_client_name or "") .. " " .. lsp_config.indicator_ok
end

local function statusline_iced()
  return "nREPL: " .. vim.fn["iced#nrepl#status"]()
end

gls.right = {
  {
    FileFormat = {
      provider = "FileFormat",
      separator = "",
      separator_highlight = {colors.bg, colors.line_bg},
      highlight = {colors.fg, colors.line_bg}
    }
  },
  -- gls.right[2] = {
  --   IcedStatus = {
  --     provider = statusline_iced,
  --     condition = function()
  --       return vim.bo.ft == "clojure" and vim.fn["iced#nrepl#status"]
  --     end,
  --     separator = " | ",
  --     separator_highlight = {colors.blue, colors.line_bg},
  --     highlight = {colors.fg, colors.line_bg}
  --   }
  -- }
  {
    LspStatus = {
      provider = statusline_lsp,
      condition = function()
        if #vim.lsp.buf_get_clients() == 0 then
          return false
        end
        return true
      end,
      separator = " | ",
      separator_highlight = {colors.blue, colors.line_bg},
      highlight = {colors.fg, colors.line_bg}
    }
  },
  {
    LineInfo = {
      provider = "LineColumn",
      separator = " | ",
      separator_highlight = {colors.blue, colors.line_bg},
      highlight = {colors.fg, colors.line_bg}
    }
  },
  {
    PerCent = {
      provider = "LinePercent",
      separator = " |",
      separator_highlight = {colors.blue, colors.line_bg},
      highlight = {colors.fg, colors.darkblue}
    }
  }
}

gls.short_line_left[1] = {
  BufferType = {
    provider = "FileTypeName",
    separator = " ",
    separator_highlight = {colors.purple, colors.bg},
    highlight = {colors.fg, colors.purple}
  }
}

gls.short_line_right[1] = {
  BufferIcon = {
    provider = "BufferIcon",
    separator = " ",
    separator_highlight = {colors.purple, colors.bg},
    highlight = {colors.fg, colors.purple}
  }
}

local M = {}

function M.on_attach(_, _)
  gl.load_galaxyline()
end

return M
