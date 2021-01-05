require 'busted.runner'()

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
end)
