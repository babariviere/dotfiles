local uv = require "luv"

local M = {}

local stdout = uv.new_tty(1, false)
local stderr = uv.new_tty(2, false)

--- Install package.
-- @param package string (package name)
-- @param opts table of options
-- @param callback function(pname, installed)
-- @return true if install is succesful
function M.install(package, opts, callback)
  opts = opts or {}

  local args = {"install", package}
  if opts.head then
    table.insert(args, "--head")
  end

  uv.spawn("brew", {args = args, stdio = {nil, stdout, stderr}}, function(code, _)
    if callback then
      callback(package, code == 0)
    end
  end)
end

--- Check if package is installed
-- @param package name
-- @param callback function(pname, installed)
-- @return true if installed
function M.installed(package, callback)
  uv.spawn("brew", {args = {"list", package}}, function(code, _)
    if callback then
      callback(package, code == 0)
    end
  end)
end

--- Update package.
-- @param package name
-- @param opts table of options
-- @param callback function(pname, updated)
-- @return true if updated
function M.update(package, opts, callback)
  opts = opts or {}

  local args = {"upgrade", package}
  if opts.head then
    table.insert(args, "--fetch-HEAD")
  end

  uv.spawn("brew", {args = args, stdio = {nil, stdout, stderr}}, function(code, _)
    if callback then
      callback(package, code == 0)
    end
  end)
end

--- Update all packages
-- @param callback function(updated)
-- @return true if updated
function M.updateAll(callback)
  uv.spawn("brew", {args = {"upgrade", "--fetch-HEAD"}, stdio = {nil, stdout, stderr}}, function(code, _)
    if callback then
      callback(code == 0)
    end
  end)
end

--- Check if package is outdated
-- @param package name
-- @param opts table of options
-- @param callback function(package, outdated)
-- @return true if outdated
function M.outdated(package, opts, callback)
  opts = opts or {}

  local args = {"outdated", package}
  if opts.head then
    table.insert(args, "--fetch-HEAD")
  end

  uv.spawn("brew", {args = args}, function(code, _)
    if callback then
      callback(package, code ~= 0)
    end
  end)
end

--- Dump installed packages in a brewfile
-- @param outputDir directory for the output file
-- @return true if done
function M.dumpInstalled(outputDir)
  uv.spawn("brew", {args = {"bundle", "dump", "-f", "--all"}, cwd = outputDir})
end

return M
