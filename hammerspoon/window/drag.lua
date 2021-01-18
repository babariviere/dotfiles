local c = require("hs.canvas")
local tap = require("hs.eventtap")
local tapevent = require("hs.eventtap.event")

local M = {window = nil, canvas = nil, frame = nil, timer = nil, _duration = 0}

local function getWindowUnderCursor()
  local pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
  local screen = hs.mouse.getCurrentScreen()

  return hs.fnutils.find(hs.window.orderedWindows(), function(w)
    return screen == w:screen() and pos:inside(w:frame())
  end)
end

local function windowSetFrame()
  M.window:setFrame(M.frame)
  M.canvas:frame(M.frame)
end

local function createCanvas()
  M.window = getWindowUnderCursor()
  if M.window == nil then
    return
  end
  M.window:focus()

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

local function deleteCanvas()
  if M.window == nil then
    return
  end
  M.timer:fire()
  M.timer:stop()
  M.timer = nil

  -- Cleanup
  M.window:focus()
  M.window = nil
  M.canvas = M.canvas:delete()
  hs.window.animationDuration = M._duration
end

local function handleMouseState(e)
  local pressure = e:getProperty(tapevent.properties.mouseEventPressure)
  local flags = e:getFlags()
  local clicked = pressure > 0

  if clicked and flags.fn then
    createCanvas()
  else
    deleteCanvas()
  end
end

--- Left click handler

local function moveWindow(e)
  if M.window == nil then
    return
  end

  local dx = e:getProperty(tapevent.properties.mouseEventDeltaX)
  local dy = e:getProperty(tapevent.properties.mouseEventDeltaY)

  M.frame.x = M.frame.x + dx
  M.frame.y = M.frame.y + dy
end

--- Right click handler

local function resizeWindow(e)
  if M.window == nil then
    return
  end

  local dx = e:getProperty(tapevent.properties.mouseEventDeltaX)
  local dy = e:getProperty(tapevent.properties.mouseEventDeltaY)

  M.frame.w = M.frame.w + dx
  M.frame.h = M.frame.h + dy
end

function M:start()
  self._mouseState = tap.new({
    tapevent.types.leftMouseUp,
    tapevent.types.leftMouseDown,
    tapevent.types.rightMouseUp,
    tapevent.types.rightMouseDown
  }, handleMouseState):start()
  self._leftDrag = tap.new({tapevent.types.leftMouseDragged}, moveWindow):start()
  self._rightDrag = tap.new({tapevent.types.rightMouseDragged}, resizeWindow):start()
end

function M:stop()
  self._mouseState:stop()
  self._leftDrag:stop()
  self._rightDrag:stop()
end

return M
