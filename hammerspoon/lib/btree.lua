-- local BTree = {}
-- BTree.__index = BTree
--
-- function BTree:new(opts)
--   opts = opts or {}
--   local tree = {root = nil}
--   setmetatable(tree, BTree)
--   return tree
-- end
local Node = {}
Node.__index = Node

function Node:new(value)
  local node = {value = value, prev = nil, next = nil}
  setmetatable(node, Node)
  return node
end

function Node:insert(value)
  if self.left == nil then
    self.left = Node:new(value)
    return
  end

  if self.right == nil then
    self.right = Node:new(value)
    return
  end

  local nearest = self:nearest()
  nearest:insert(value)
end

-- Return the nearest node in branch
function Node:nearest()
  local function findNearest(node)
    if node.left == nil or node.right == nil then return node, 1 end

    local l, llen = findNearest(node.left)
    local r, rlen = findNearest(node.right)
    if llen <= rlen then
      return l, llen + 1
    else
      return r, rlen + 1
    end
  end

  if self.left == nil or self.right == nil then return self end

  local l, llen = findNearest(self.left)
  local r, rlen = findNearest(self.right)

  if llen <= rlen then
    return l
  else
    return r
  end
end

-- Find node with value.
function Node:find(value)
  if self.value == value then return self end

  if self.left ~= nil then
    local found = self.left:find(value)
    if found then return found end
  end

  if self.right ~= nil then
    local found = self.right:find(value)
    if found then return found end
  end
end

function Node:__tostring()
  local function prettyprint(node, depth, prefix)
    local prefix = prefix or 'R '
    local s = string.rep(' ', depth) .. prefix .. node.value .. '\n'
    if node.left ~= nil then s = s .. prettyprint(node.left, depth + 1, 'l ') end
    if node.right ~= nil then
      s = s .. prettyprint(node.right, depth + 1, 'r ')
    end
    return s
  end

  return prettyprint(self, 0)
end

return Node
