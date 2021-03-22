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

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 40 then
    return true
  end
  return false
end

gls.left[5] = {
  LeftEnd = {
    provider = function()
      return ""
    end,
    separator = "",
    separator_highlight = {colors.bg, colors.line_bg},
    highlight = {colors.line_bg, colors.line_bg}
  }
}
gls.left[6] = {DiagnosticError = {provider = "DiagnosticError", icon = "  ", highlight = {colors.red, colors.bg}}}
gls.left[7] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = {colors.bg, colors.bg}
  }
}
gls.left[8] = {DiagnosticWarn = {provider = "DiagnosticWarn", icon = "  ", highlight = {colors.blue, colors.bg}}}
gls.left[9] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = {colors.bg, colors.bg}
  }
}

gls.right = {
  {
    LspStatus = {
      provider = "GetLspClient",
      condition = function()
        local tbl = {["dashboard"] = true, [""] = true}
        if tbl[vim.bo.filetype] then
          return false
        end
        return true
      end,
      separator = " ",
      separator_highlight = {colors.blue, colors.line_bg},
      icon = "異",
      highlight = {colors.fg, colors.line_bg}
    }
  },
  {
    Space = {
      provider = function()
        return " "
      end,
      separator = " "
    }
  },
  {
    DiffAdd = {
      provider = "DiffAdd",
      condition = checkwidth,
      icon = " ",
      highlight = {"#09f7a0", colors.line_bg}
    }
  },
  {
    DiffModified = {
      provider = "DiffModified",
      condition = checkwidth,
      icon = " ",
      highlight = {"#21bfc2", colors.line_bg}
    }
  },
  {
    DiffRemove = {
      provider = "DiffRemove",
      condition = checkwidth,
      icon = " ",
      highlight = {"#f43e5c", colors.line_bg}
    }
  },
  {
    GitIcon = {
      provider = function()
        return "  "
      end,
      condition = find_git_root,
      highlight = {colors.orange, colors.line_bg}
    }
  },
  {
    GitBranch = {
      provider = "GitBranch",
      condition = find_git_root,
      highlight = {colors.fg, colors.line_bg, "bold"}
    }
  }
  -- {
  --   LineInfo = {
  --     provider = "LineColumn",
  --     separator = " ",
  --     separator_highlight = {colors.blue, colors.line_bg},
  --     highlight = {colors.fg, colors.line_bg}
  --   }
  -- },
  -- {
  --   PerCent = {
  --     provider = "LinePercent",
  --     separator = " ",
  --     separator_highlight = {colors.blue, colors.line_bg},
  --     highlight = {colors.fg, colors.darkblue}
  --   }
  -- }
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
