local c = require('hs.canvas')

local drag = {
  window = nil,
  canvas = nil,
  isPressed = false,
  frame = nil,
  timer = nil,
  _duration = 0
}

local function windowSetFrame()
  drag.window:setFrame(drag.frame)
  drag.canvas:frame(drag.frame)
end

--- FN key handler

local function onFnPressed()
  drag.window = hs.window.frontmostWindow()

  drag._duration = hs.window.animationDuration
  hs.window.animationDuration = 0

  drag.canvas = c.new(drag.window:frame())
  drag.canvas[1] = {type = 'rectangle', id = 'part1', action = 'build'}
  drag.canvas:clickActivating(false)
  drag.canvas:canvasMouseEvents(true, true, false, false)
  -- Required to disable mouse event for focused window below
  drag.canvas:mouseCallback(function(_, _, _, _, _) end)
  drag.canvas:level('dragging')
  drag.canvas:show()

  drag.frame = drag.window:frame()
  drag.timer = hs.timer.doEvery(0.01, windowSetFrame):start()
end

local function onFnReleased()
  drag.timer:fire()
  drag.timer:stop()

  -- Cleanup
  drag.window:focus()
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
  return true
end

--- Left click handler

local function moveWindow(e)
  if not drag.isPressed or drag.window == nil then return end
  local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
  local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)

  drag.frame.x = drag.frame.x + dx
  drag.frame.y = drag.frame.y + dy
end

--- Right click handler

local function resizeWindow(e)
  if not drag.isPressed or drag.window == nil then return end
  local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
  local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)

  drag.frame.w = drag.frame.w + dx
  drag.frame.h = drag.frame.h + dy
end

function drag:start()
  self._fn = hs.eventtap
               .new({hs.eventtap.event.types.flagsChanged}, handleFnKey):start()
  self._leftDrag = hs.eventtap.new({hs.eventtap.event.types.leftMouseDragged},
                                   moveWindow):start()
  self._rightDrag = hs.eventtap.new({hs.eventtap.event.types.rightMouseDragged},
                                    resizeWindow):start()
end

function drag:stop()
  self._fn = self._fn:stop()
  self._leftDrag:stop()
  self._rightDrag:stop()
end

return drag
