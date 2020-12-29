local M = {}

-- Tile all windows on current screen.
function M.tile()
  -- TODO(babariviere): we may need to restrict to current screen
  local screen = hs.screen.mainScreen()
  local windows = hs.window.visibleWindows()

  local boundary = screen:frame()
  local windowsF = {}

  print(#windows)
  print(hs.inspect(windows))
  for i, window in pairs(windows) do
    local frame = hs.geometry.copy(boundary)
    local prevw = windows[i - 1]
    if prevw then
      local prev = windowsF[prevw:id()]
      if prev.w > prev.h then
        prev.w = prev.w / 2.
        frame.x = prev.x + prev.w
        frame.y = prev.y
      else
        prev.h = prev.h / 2.
        frame.x = prev.x
        frame.y = prev.y + prev.h
      end
      frame.w = prev.w
      frame.h = prev.h
      windowsF[prevw:id()] = prev
    end
    windowsF[window:id()] = frame
  end

  for _, window in pairs(windows) do
    print(window:application():name() .. ' ' .. windowsF[window:id()].string)
    window:setFrame(windowsF[window:id()])
    print(window:application():name() .. ' ' .. window:frame().string)
  end
end

function M.start() hs.hotkey.bind({'cmd', 'alt'}, 'k', 'Tile', M.tile) end

function M.stop() end

return M
