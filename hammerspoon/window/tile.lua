local module = {
  -- Windows in spaces
  spaces = {},
  assets = {},
  rejectApps = {"Notification Centre", "Alfred", "1password", "Hammerspoon"},

  --- Options
  menubar = true
}

local bsp = require("lib/bsp")
local spaces = require("hs._asm.undocumented.spaces")
local wf = require("hs.window.filter")
local mb = require("hs.menubar")

local function moveWindow()
  local delay = 0.

  return function(node)
    if not node.window then
      return
    end
    if node.rect == node.window:frame() then
      return
    end
    hs.timer.doAfter(delay, function()
      node.window:setFrame(node.rect)
    end)
    delay = delay + 0.2
  end
end

local function onWindowCreated(window, _, _)
  local space = window:spaces()[1]

  local root = module.spaces[space]
  if not root then
    return
  end

  root:insertWindow(window)

  root:forEachLeaf(moveWindow())
end

local function onWindowDestroyed(window, _, _)
  local space = spaces.activeSpace()

  local root = module.spaces[space]
  if not root then
    return
  end

  root:deleteWindow(window)

  root:forEachLeaf(moveWindow())
end

local function onWindowFocused(window, _, _)
  local space = window:spaces()[1]
  if module.spaces[space] then
    module._menubar:setIcon(module.assets.active)
  else
    module._menubar:setIcon(module.assets.inactive)
  end
end

function module.toggleTile()
  local space = spaces.activeSpace()
  if module.spaces[space] then
    module.spaces[space] = nil
    module._menubar:setIcon(module.assets.inactive)
  else
    module.tile()
  end
end

local function isAllowed(win)
  local appname = win:application():name()
  for _, v in pairs(module.rejectApps) do
    if v == appname then
      return false
    end
  end

  return true
end

function module.tile()
  local screen = hs.screen.mainScreen()
  local space = spaces.activeSpace()
  local windows = spaces.allWindowsForSpace(space)

  local root = bsp.new(screen:frame()) -- root is the screen, it should not be resized

  for _, window in pairs(windows) do
    if isAllowed(window) then
      root:insertWindow(window)
    end
  end

  root:forEachLeaf(moveWindow())

  module.spaces[spaces.activeSpace()] = root
  module._menubar:setIcon(module.assets.active)
end

function module.setupAssets()
  module.assets = {}
  module.assets.active = hs.image.imageFromPath(hs.configdir .. "/assets/tile_active.png"):size({w = 16, h = 16})
  module.assets.inactive = hs.image.imageFromPath(hs.configdir .. "/assets/tile_inactive.png"):size({w = 16, h = 16})
end

function module.createMenubar()
  module._menubar = mb.new()
  module._menubar:setClickCallback(module.toggleTile)
  module._menubar:setIcon(module.assets.inactive)
  module._menubar:setTooltip("Toggle Tile")
end

function module.start()
  module.setupAssets()

  hs.hotkey.bind({"cmd", "alt"}, "k", "Tile", module.tile)
  hs.hotkey.bind({"cmd", "alt"}, "j", "Toggle Tile", module.toggleTile)

  module._wf = wf.new()

  for _, app in pairs(module.rejectApps) do
    module._wf = module._wf:rejectApp(app)
  end

  -- TODO(babariviere): reject app only for "window created" event
  module._wf = module._wf:subscribe(wf.windowCreated, onWindowCreated):subscribe(
                 {wf.windowDestroyed, wf.windowHidden, wf.windowMinimized}, onWindowDestroyed):subscribe(
                 {wf.windowFocused}, onWindowFocused)

  if module.menubar then
    module.createMenubar()
  end
end

function module.stop()
  module._wf:unsubscribeAll()

  if module._menubar then
    module._menubar:delete()
  end
end

return module
