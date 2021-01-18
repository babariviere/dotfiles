local c = require("hs.canvas")

local M = {window = nil, canvas = nil, isPressed = false, frame = nil, timer = nil, _duration = 0}

local function windowSetFrame()
  M.window:setFrame(M.frame)
  M.canvas:frame(M.frame)
end

local function getWindowUnderCursor()
  local pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
  local screen = hs.mouse.getCurrentScreen()

  return hs.fnutils.find(hs.window.orderedWindows(), function(w)
    return screen == w:screen() and pos:inside(w:frame())
  end)
end

--- FN key handler

local function onFnPressed()
  M.window = getWindowUnderCursor()

  M._duration = hs.window.animationDuration
  hs.window.animationDuration = 0

  M.canvas = c.new(M.window:frame())
  M.canvas[1] = {type = "rectangle", id = "part1", action = "build"}
  M.canvas:clickActivating(false)
  M.canvas:canvasMouseEvents(true, true, false, false)
  -- Required to disable mouse event for focused window below
  M.canvas:mouseCallback(function(_, _, _, _, _)
  end)
  M.canvas:level("dragging")
  M.canvas:show()

  M.frame = M.window:frame()
  M.timer = hs.timer.doEvery(0.01, windowSetFrame):start()
end

local function onFnReleased()
  M.timer:fire()
  M.timer:stop()

  -- Cleanup
  M.window:focus()
  M.window = nil
  M.canvas = M.canvas:delete()
  hs.window.animationDuration = M._duration
end

local function handleFnKey(e)
  local isPressed = e:getFlags()["fn"] or false

  if isPressed == M.isPressed then
    return
  end

  M.isPressed = isPressed
  if isPressed then
    onFnPressed()
  else
    onFnReleased()
  end
  return true
end

--- Left click handler

local function moveWindow(e)
  if not M.isPressed or M.window == nil then
    return
  end
  local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
  local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)

  M.frame.x = M.frame.x + dx
  M.frame.y = M.frame.y + dy
end

--- Right click handler

local function resizeWindow(e)
  if not M.isPressed or M.window == nil then
    return
  end
  local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
  local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)

  M.frame.w = M.frame.w + dx
  M.frame.h = M.frame.h + dy
end

function M:start()
  self._fn = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, handleFnKey):start()
  self._leftDrag = hs.eventtap.new({hs.eventtap.event.types.leftMouseDragged}, moveWindow):start()
  self._rightDrag = hs.eventtap.new({hs.eventtap.event.types.rightMouseDragged}, resizeWindow):start()
end

function M:stop()
  self._fn = self._fn:stop()
  self._leftDrag:stop()
  self._rightDrag:stop()
end

return M
