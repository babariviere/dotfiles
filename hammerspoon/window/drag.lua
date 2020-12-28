local c = require('hs.canvas')

local drag = {
  window = nil,
  canvas = nil,
  isPressed = false,
  coords = nil,
  _duration = 0
}

--- FN key handler

local function onFnPressed()
  drag.window = hs.window.focusedWindow()

  drag._duration = hs.window.animationDuration
  hs.window.animationDuration = 0

  drag.canvas = c.new(drag.window:frame()):show()
  drag.canvas[1] = {type = 'rectangle', id = 'part1', action = 'clip'}
  drag.canvas:clickActivating(true)
  drag.canvas:canvasMouseEvents(true, true, false, false)
  drag.canvas:mouseCallback(function(_, _, _, _, _) end)
end

local function onFnReleased()
  drag.window = nil
  drag.canvas = drag.canvas:delete()
  hs.window.animationDuration = drag._duration
end

local function handleFnKey(e)
  local isPressed = e:getFlags()['fn'] or false

  if isPressed == drag.isPressed then return end

  drag.isPressed = isPressed
  if isPressed then
    onFnPressed()
  else
    onFnReleased()
  end
  return false
end

--- Left click handler

local function moveWindow(e)
  if not drag.isPressed or drag.window == nil then return end
  local f = drag.window:frame()
  f.x = f.x + e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
  f.y = f.y + e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)
  drag.window:setFrame(f)
  drag.canvas:frame(f)
end

function drag:start()
  self._fn = hs.eventtap
               .new({hs.eventtap.event.types.flagsChanged}, handleFnKey):start()
  self._drag = hs.eventtap.new({hs.eventtap.event.types.leftMouseDragged},
                               moveWindow):start()
end

function drag:stop()
  self._fn = self._fn:stop()
  self._drag:stop()
end

return drag
