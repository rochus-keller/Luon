--[[
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*
* Alternatively this file may be used under the terms of the Mozilla 
* Public License. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/.
]]--

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

