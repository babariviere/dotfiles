local M = {
  -- Configuration
  home = os.getenv("HOME"),
  dot_dir = os.getenv("HOME") .. "/src/github.com/babariviere/dotfiles" -- TODO(babariviere): I don't like it but it should do it for now
}

--- Returns the list of enabled packages.
-- @return table of string
function M.packages()
  return M._packages
end

--- Load packages into the builder.
-- This will overwrite previously loaded packages.
--
-- @param packages table of string
-- @return self
function M.loadPackages(...)
  M._packages = {}
  -- TODO(babariviere): allow for option in packages?
  -- e.g neovim = { head = true }
  for i = 1, select("#", ...) do
    local pname = select(i, ...)
    M._packages[pname] = require("packages." .. pname)
  end
  return M
end

local function handleInstall(pname, installed)
  if installed then
    print(pname .. " is now installed.")
  else
    print(pname .. " has failed installing.")
  end
end

--- Install missing packages.
function M.installPackages()
  -- TODO(babariviere): allow to give an optional list
  for _, package in pairs(M._packages) do
    package.installed(function(pname, installed)
      if installed then
        print(pname .. " is already installed.")
      else
        package.install(handleInstall)
      end
    end)
  end
end

local function handleUpdate(pname, updated)
  if updated then
    print(pname .. " is now updated.")
  else
    print(pname .. " has failed updating.")
  end
end

--- Update outdated packages.
function M.updatePackages()
  -- TODO(babariviere): allow to give an optional list of packages
  for _, package in pairs(M._packages) do
    package.outdated(function(pname, outdated)
      if outdated then
        package.update(handleUpdate)
      else
        print(pname .. " is up to date")
      end
    end)
  end
end

--- Link configuration files for packages.
function M.linkPackages()
  for _, package in pairs(M._packages) do
  end
end

return M
