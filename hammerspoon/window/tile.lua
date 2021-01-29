local module = {
  -- Windows in spaces
  spaces = {},
  assets = {},
  rejectApps = {"Notification Centre", "Alfred", "1Password 7", "Hammerspoon", "Slack"},

  --- Options
  menubar = true
}

local cache = {}

local bsp = require("lib/bsp")
local spaces = require("hs._asm.undocumented.spaces")
local wf = require("hs.window.filter")
local mb = require("hs.menubar")

-- TODO(babariviere): cleaning
-- TODO(babariviere): still a bug to fix when starting computer
--  tiling is applied on 2 spaces (allWindowsForSpace doesn't seems to work but it's faster anyway)
--  we need to do some logging at boot
-- TODO(babariviere): bug when I change windows to another space

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

  -- Avoid duplcation issue in tree
  if not root:findLeaf(window:id()) then
    root:insertWindow(window)
  end

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
    cache.menubar:setIcon(module.assets.active)
  else
    cache.menubar:setIcon(module.assets.inactive)
  end
end

function module.toggleTile()
  local space = spaces.activeSpace()
  if module.spaces[space] then
    module.spaces[space] = nil
    cache.menubar:setIcon(module.assets.inactive)
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
  cache.menubar:setIcon(module.assets.active)
end

function module.setupAssets()
  module.assets = {}
  module.assets.active = hs.image.imageFromPath(hs.configdir .. "/assets/tile_active.png"):size({w = 16, h = 16})
  module.assets.inactive = hs.image.imageFromPath(hs.configdir .. "/assets/tile_inactive.png"):size({w = 16, h = 16})
end

function module.createMenubar()
  cache.menubar = mb.new()
  cache.menubar:setClickCallback(module.toggleTile)
  cache.menubar:setIcon(module.assets.inactive)
  cache.menubar:setTooltip("Toggle Tile")
end

function module.start()
  module.setupAssets()

  hs.hotkey.bind({"cmd", "alt"}, "k", nil, module.tile)
  hs.hotkey.bind({"cmd", "alt"}, "j", nil, module.toggleTile)

  cache.wfcreated = wf.new()

  for _, app in pairs(module.rejectApps) do
    cache.wfcreated = cache.wfcreated:rejectApp(app)
  end

  -- TODO(babariviere): reject app only for "window created" event
  -- TODO(babariviere): investigate event not triggered (we are forced to reboot hammerspoon)
  cache.wf = wf.new():setDefaultFilter():subscribe({wf.windowDestroyed, wf.windowHidden, wf.windowMinimized},
                                                   onWindowDestroyed):subscribe({wf.windowFocused}, onWindowFocused)
  cache.wfcreated = cache.wfcreated:subscribe({wf.windowCreated, wf.windowUnminimized}, onWindowCreated)

  if module.menubar then
    module.createMenubar()
  end
end

function module.stop()
  cache.wf:unsubscribeAll()
  cache.wfcreated:unsubscribeAll()

  if cache.menubar then
    cache.menubar:delete()
  end
end

return module
