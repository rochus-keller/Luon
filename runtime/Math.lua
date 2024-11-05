local module = {}
local math = require("math")

-- TODO

function module.round(x)
	return math.floor(x+0.5)
end

function module.log(x, base)
  return math.log(x) / math.log(base)
end

function module.arcsinh(x)
  return math.log(x + math.sqrt(x*x + 1))
end

function module.arccosh(x)
  if x < 1 then
    error("arccosh is only defined for x >= 1")
  end
  return math.log(x + math.sqrt(x*x - 1))
end

function module.arctanh(x)
  if x <= -1 or x >= 1 then
    error("arctanh is only defined for -1 < x < 1")
  end
  return 0.5 * math.log((1 + x) / (1 - x))
end

-- NOTE: these numbers are allocated as by LjbcGen and need to be updated if Math.luon changes!
module[0] = 3.14159265358979323846
module[1] = 2.71828182845904523536
module[2] = math.sqrt
module[3] = math.pow
module[4] = math.exp
module[5] = math.log
module[6] = module.log 
module[7] = module.round
module[8] = math.sin
module[9] = math.cos
module[10] = math.tan
module[11] = math.asin
module[12] = math.acos
module[13] = math.atan
module[14] = math.atan2
module[15] = math.sinh
module[16] = math.cosh
module[17] = math.tanh
module[18] = module.arcsinh
module[19] = module.arccosh
module[20] = module.arctanh

return module
