local M = {}

local bsp = require('lib/bsp')

function M.tile()
  local screen = hs.screen.mainScreen()
  local filter = hs.window.filter.new():setCurrentSpace(true):setScreens(
                   {screen:id()}):rejectApp('Notification Centre')
  local windows = filter:getWindows()

  local root = bsp.new(screen:frame()) -- root is the screen, it should not be resized

  for _, window in pairs(windows) do root:insertWindow(window) end

  local delay = 0.
  root:forEachLeaf(function(node)
    if node.rect == node.window:frame() then return end
    hs.timer.doAfter(delay, function() node.window:setFrame(node.rect) end)
    delay = delay + 0.2
  end)
end

function M.start() hs.hotkey.bind({'cmd', 'alt'}, 'k', 'Tile', M.tile) end

function M.stop() end

return M
