local builder = require "builder"
local uv = require "luv"

builder.loadPackages("neovim")
builder.updatePackages()

uv.run()
