local function package_path()
  local home = os.getenv("HOME")
  return {
    home .. "/.hammerspoon/?.lua",
    home .. "/.hammerspoon/?/init.lua",
    home .. "/.hammerspoon/Spoons/?.spoon/init.lua",
    "/usr/local/share/lua/5.4/?.lua",
    "/usr/local/share/lua/5.4/?/init.lua",
    "/usr/local/lib/lua/5.4/?.lua",
    "/usr/local/lib/lua/5.4/?/init.lua",
    "./?.lua",
    "./?/init.lua",
    "/Applications/Hammerspoon.app/Contents/Resources/extensions/?.lua",
    "/Applications/Hammerspoon.app/Contents/Resources/extensions/?/init.lua"
  }
end

return {
  Lua = {
    runtime = {version = "Lua 5.4", path = package_path()},
    diagnostics = {globals = {"hs"}},
    workspace = {library = {["/Applications/Hammerspoon.app/Contents/Resources/extensions"] = true}}
  }
}
