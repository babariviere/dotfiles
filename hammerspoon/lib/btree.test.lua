require 'busted.runner'()
local inspect = require 'inspect'

describe('Binary tree', function()
  setup(function() btree = require('./lib/btree') end)

  teardown(function() btree = nil end)

  it('should insert nodes', function()
    local root = btree:new(1)
    root:insert(2)
    root:insert(3)
    root:insert(4)
    root:insert(5)
    root:insert(6)

    assert.are.equal(root.left.value, 2)
    assert.are.equal(root.left.value, 2)
    assert.are.equal(root.right.value, 3)
    assert.are.equal(root.left.left.value, 4)
    assert.are.equal(root.left.right.value, 5)
    assert.are.equal(root.right.left.value, 6)

    root.left, root.right = root.right, root.left

    root:insert(7)
    root:insert(8)
    root:insert(9)

    assert.are.equal(root.right.left.value, 4)
    assert.are.equal(root.right.right.value, 5)
    assert.are.equal(root.left.left.value, 6)
    assert.are.equal(root.left.right.value, 7)
    assert.are.equal(root.left.left.left.value, 8)
    assert.are.equal(root.left.left.right.value, 9)
  end)

  it('should find nodes', function()
    local root = btree:new(1)
    root:insert(2) -- left
    root:insert(3) -- right
    root:insert(4) -- left left
    root:insert(5) -- left right
    root:insert(6) -- right left

    assert.are.equal(root:find(2).left.value, 4)
    assert.are.equal(root:find(2).right.value, 5)
    assert.are.equal(root:find(6).left, nil)
  end)

  it('should traverse tree', function()
    local root = btree:new('h')
    root:insert('e') -- left
    root:insert('o') -- right
    root:insert('l') -- left left
    root:insert('l') -- left right

    local res = ''
    root:traverse(function(node, parent, depth)
      res = res .. node.value .. tostring(depth)
      if parent ~= nil then res = res .. parent.value end
      return depth + 1
    end, 1)

    assert.are.equal(res, 'h1e2hl3el3eo2h')
  end)

  it('should map tree', function()
    local root = btree:new(1)
    root:insert(2) -- left
    root:insert(3) -- right
    root:insert(4) -- left left
    root:insert(5) -- left right
    root:insert(6) -- right left

    local new = root:map(function(node, parent)
      if parent == nil then return node.value end

      return node.value + parent.value
    end)

    assert.are.equal(new.value, root.value)
    assert.are.equal(new.left.value, new.value + root.left.value)
    assert.are.equal(new.left.left.value, new.left.value + root.left.left.value)
    assert.are.equal(new.left.right.value,
                     new.left.value + root.left.right.value)
    assert.are.equal(new.right.value, new.value + root.right.value)
    assert.are.equal(new.right.left.value,
                     new.right.value + root.right.left.value)
  end)
end)
