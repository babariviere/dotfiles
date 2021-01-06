local Node = {}
Node.__index = Node

function Node.new(rect, ratio)
  local node = {
    firstChild = nil,
    secondChild = nil,
    parent = nil,
    window = nil, -- only set when window is leaf
    rect = rect,
    ratio = ratio or 0.5
  }
  setmetatable(node, Node)
  return node
end

function Node:isRoot() return self.parent == nil end

function Node:isLeaf() return self.firstChild == nil and self.secondChild == nil end

function Node:insertWindow(window)
  -- TODO: when is leaf, split and create new node
  -- when not leaf, find biggest rect
  if not self:isLeaf() then
    local biggest = self:biggestLeaf()
    biggest:insertWindow(window)
    return
  end

  if self.window == nil then
    self.window = window
    return
  end

  local firstRect, secondRect = hs.geometry.copy(self.rect),
                                hs.geometry.copy(self.rect)

  if firstRect.w > firstRect.h then
    firstRect.w = firstRect.w * self.ratio
    secondRect.x = firstRect.x + firstRect.w
  else
    firstRect.h = firstRect.h * self.ratio
    secondRect.y = firstRect.y + firstRect.h
  end
  secondRect.w = firstRect.w
  secondRect.h = firstRect.h

  local first = Node.new(firstRect, 0.5) -- TODO: configurable ratio
  first.parent = self
  first.window = self.window
  self.window = nil

  local second = Node.new(secondRect, 0.5)
  second.parent = self
  second.window = window

  self.firstChild = first
  self.secondChild = second
end

-- Return biggest leaf
function Node:biggestLeaf()
  if not self:isLeaf() then
    local first = self.firstChild:biggestLeaf()
    local second = self.secondChild:biggestLeaf()
    if first.rect.area > second.rect.area then
      return first
    else
      return second
    end
  end

  return self
end

-- Execute function for each leaf
function Node:forEachLeaf(f)
  if self:isLeaf() then
    f(self)
    return
  end

  self.firstChild:forEachLeaf(f)
  self.secondChild:forEachLeaf(f)
end

return Node
