local uv = require "luv"

uv.spawn("ls", {args = {"-al"}}, function(code)
  print(code)
end)

uv.run()
