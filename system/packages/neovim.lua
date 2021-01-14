local brew = require "utils.brew"

local M = {}

function M.installed(callback)
  return brew.installed("neovim", callback)
end

function M.install(callback)
  return brew.install("neovim", {head = true}, callback)
end

function M.outdated(callback)
  brew.outdated("neovim", {head = true}, callback)
end

function M.update(callback)
  brew.update("neovim", {head = true}, callback)
end

-- Sync files
function M.sync()

end

M.configPath = "/config/nvim"
M.outPath = "/.config/nvim"

return M
