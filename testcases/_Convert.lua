local ffi = require 'ffi'
local C = ffi.C

ffi.cdef[[
	typedef union Convert_float_int {
		uint32_t i;
		float f;
	} Convert_float_int;
]]

local helper = ffi.new( ffi.typeof("Convert_float_int") )

function Convert_realToInt(x)
	helper.f = x
	return helper.i
end

function Convert_intToReal(x)
	helper.i = x
	return helper.f
end
