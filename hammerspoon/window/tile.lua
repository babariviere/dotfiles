local module = {
  -- Windows in spaces
  spaces = {}
}

local bsp = require('lib/bsp')
local spaces = require('hs._asm.undocumented.spaces')
local wf = require('hs.window.filter')

local function moveWindow()
  local delay = 0.

  return function(node)
    if node.rect == node.window:frame() then return end
    hs.timer.doAfter(delay, function()
      node.window:setFrame(node.rect)
    end)
    delay = delay + 0.2
  end
end

local function onWindowCreated(window, _, _)
  local space = window:spaces()[1]

  local root = module.spaces[space]
  if not root then return end

  root:insertWindow(window)

  root:forEachLeaf(moveWindow())
end

local function onWindowDestroyed(window, _, _)
  local space = spaces.activeSpace()

  local root = module.spaces[space]
  if not root then return end

  root:deleteWindow(window)

  root:forEachLeaf(moveWindow())
end

function module.tile()
  local screen = hs.screen.mainScreen()
  local filter = wf.new():setCurrentSpace(true):setScreens({screen:id()}):rejectApp('Notification Centre')
  local windows = filter:getWindows()

  local root = bsp.new(screen:frame()) -- root is the screen, it should not be resized

  for _, window in pairs(windows) do root:insertWindow(window) end

  root:forEachLeaf(moveWindow())

  module.spaces[spaces.activeSpace()] = root
end

function module.start()
  hs.hotkey.bind({'cmd', 'alt'}, 'k', 'Tile', module.tile)

  module._onCreate = wf.new():subscribe(wf.windowCreated, onWindowCreated)
  module._onDestroy = wf.new():subscribe({wf.windowDestroyed, wf.windowHidden, wf.windowMinimized}, onWindowDestroyed)
end

function module.stop()
  module._onCreate:unsubscribeAll()
end

return module
