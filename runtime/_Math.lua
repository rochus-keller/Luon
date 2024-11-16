local math = require("math")

Math_sqrt = math.sqrt
Math_pow = math.pow
Math_exp = math.exp
Math_log = math.log
Math_sin = math.sin
Math_cos = math.cos
Math_tan = math.tan
Math_asin = math.asin
Math_acos = math.acos
Math_atan = math.atan
Math_atan2 = math.atan2
Math_sinh = math.sinh
Math_cosh = math.cosh
Math_tanh = math.tanh

function Math_round(x)
	return math.floor(x+0.5)
end

function Math_log(x, base)
  return math.log(x) / math.log(base)
end

function Math_arcsinh(x)
  return math.log(x + math.sqrt(x*x + 1))
end

function Math_arccosh(x)
  if x < 1 then
    error("arccosh is only defined for x >= 1")
  end
  return math.log(x + math.sqrt(x*x - 1))
end

function Math_arctanh(x)
  if x <= -1 or x >= 1 then
    error("arctanh is only defined for -1 < x < 1")
  end
  return 0.5 * math.log((1 + x) / (1 - x))
end

