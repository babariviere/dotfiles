local Node = {}
Node.__index = Node

-- TODO: better way to modify nodes without forgetting values (e.g. id)

function Node.new(rect, ratio)
  local node = {
    firstChild = nil,
    secondChild = nil,
    parent = nil,
    window = nil, -- only set when window is leaf
    id = nil,
    rect = rect:floor(),
    ratio = ratio or 0.5
  }
  setmetatable(node, Node)
  return node
end

function Node:isRoot()
  return self.parent == nil
end

function Node:isLeaf()
  return self.firstChild == nil and self.secondChild == nil
end

function Node:insertWindow(window)
  if not self:isLeaf() then
    local biggest = self:biggestLeaf()
    biggest:insertWindow(window)
    return
  end

  if self.window == nil then
    self.window = window
    self.id = window:id()
    return
  end

  local firstRect, secondRect = hs.geometry.copy(self.rect), hs.geometry.copy(self.rect)

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
  first.id = self.id
  self.window = nil
  self.id = nil

  local second = Node.new(secondRect, 0.5)
  second.parent = self
  second.window = window
  second.id = window:id()

  self.firstChild = first
  self.secondChild = second
end

function Node:deleteWindow(window)
  local leaf = self:findLeaf(window:id())
  if not leaf then
    return
  end

  if not leaf.parent then
    leaf.window = nil
    leaf.id = nil
    return
  end

  local parent = leaf.parent

  if parent.firstChild == self then
    parent.window = parent.secondChild.window
    parent.id = parent.secondChild.id
    parent.firstChild = nil
    parent.secondChild = nil
  else
    parent.window = parent.firstChild.window
    parent.id = parent.firstChild.id
    parent.firstChild = nil
    parent.secondChild = nil
  end
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

-- Find leaf in tree by it's id
function Node:findLeaf(leaf)
  if not self:isLeaf() then
    local first = self.firstChild:findLeaf(leaf)
    if first then
      return first
    end
    local second = self.secondChild:findLeaf(leaf)
    if second then
      return second
    end
  end

  if self.id == leaf then
    return self
  end

  return nil
end

-- Execute function for each leaf
function Node:forEachLeaf(f)
  if self:isLeaf() then
    if self.window then
      f(self)
    end
    return
  end

  self.firstChild:forEachLeaf(f)
  self.secondChild:forEachLeaf(f)
end

return Node
