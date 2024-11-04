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
]]--

local ffi = require 'ffi'
local C = ffi.C
local string = require 'string'
local io = require 'io'
local math = require 'math'
local bit = require 'bit'
local os = require 'os'
local jit = require 'jit'

local module = {}
LUON = module -- directly publish the module by global var

ffi.cdef[[
	typedef uint8_t CharArray[?];
	int LuonFfi_DIV( int64_t a, int64_t b );
        int LuonFfi_MOD( int64_t a, int64_t b );
	int LuonFfi_strRelOp( char* lhs, char* rhs, int op ); 
	void LuonFfi_DBGTRACE( const char* str );
	void LuonFfi_CRASH(int);
	void LuonFfi_TRACE( const char* str );
	void LuonFfi_NOP();
]]

local CharArray = ffi.typeof("CharArray")
local bytesize = ffi.sizeof
local frexp = math.frexp

function module.charToStringArray(len, str)
	local a = ffi.new( CharArray, len ) 
	if str then
            ffi.copy(a, str, len+1)
	end
	return a
end
function module.createLuaArray(len)
	local a = { count = len }
	return a
end
local function addElemToSet( set, elem )
	return bit.bor( set, bit.lshift( 1, elem ) )
end
function module.removeElemFromSet( set, elem )
	return bit.band( set, bit.bnot( bit.lshift( 1, elem ) ) )
end
function module.addRangeToSet( set, from, to )
	if from > to then
		return set
	end
	for i=from,to do
		set = addElemToSet(set,i)
	end
	return set
end
local function strlen( str )
	local count = bytesize(str)
	for i=0,count-1 do
		if str[i] == 0 then
			return i
		end
	end
	return count
end
function module.joinStrings( lhs, rhs)
        local lhslen = strlen(lhs)
        local rhslen = strlen(rhs)
	local count = lhslen + rhslen + 1
	local res
	res = ffi.new( CharArray, count )
	local i
	for i = 0,lhslen-1 do
		res[i] = lhs[i]
	end
	for i = 0,rhslen-1 do
		res[i+lhslen] = rhs[i]
	end
	res[lhslen+rhslen] = 0
	return res
end
function module.charToString(ch)
	local a = ffi.new( CharArray, 2 ) 
	a[0] = ch
	a[1] = 0
	return a
end
function module.stringRelOp( lhs, rhs, op )
	return C.LuonFfi_strRelOp(lhs,rhs,op) ~= 0
end
function module.setSub( lhs, rhs )
	rhs = bit.bnot(rhs)
	return bit.band( lhs, rhs )
end
function module.setDiv( lhs, rhs )
	local tmp1 = bit.bnot( bit.band( lhs, rhs ) )
	local tmp2 = bit.bor( lhs, rhs )
	return bit.band( tmp1, tmp2 )
end
function module.setTest( elem, set )
	return bit.band( set, bit.lshift( 1, elem ) ) ~= 0
end
function module.is_a( obj, class )
	local meta = getmetatable(obj)
	while meta and class and meta ~= class do
		meta = getmetatable(meta)
	end
	return meta == class
end
function module.println( val )
	if ffi.istype(CharArray,val) then
            print(ffi.string(val))
	else
            print(tostring(val))
	end
end
function module.isFfiString( val )
	return ffi.istype(CharArray,val)
end
function module.strcpy( lhs, rhs )
	local i = 0
	while rhs[i] ~= 0 do
		lhs[i] = rhs[i]
		i = i + 1
	end
	lhs[i] = 0
end
function module.ODD(num)
	return ( num % 2 ) == 1
end
function module.bool_to_number(value)
  return value and 1 or 0
end
function module.UNPACK(value)
	local x,n = frexp(value)
	x = x + x
	n = n - 1
	return x, n
end
function module.min_size(lhs,rhs)
	local l = bytesize(lhs)
	local r = bytesize(rhs)
	if l <= r then
		return l
	else
		return r
	end
end
local firstLock = true
function module.ldmod(name)
	if firstLock then
		firstLock = false
		return false
	end
	local m = require(ffi.string(name))
	-- print("LDMOD "..ffi.string(name).." "..tostring(m ~= nil)) 
	return m ~= nil
end
function module.ldcmd(mod,cmd)
	local m = require(ffi.string(mod))
	if m then return m[ffi.string(cmd)] end
	return nil
end

-- Magic mumbers used by the compiler
module[1] = module.charToStringArray
module[9] = addElemToSet
module[10] = module.addRangeToSet
module[11] = bit.bnot
module[12] = bit.bor
module[13] = module.joinStrings
module[14] = C.LuonFfi_DIV
module[15] = C.LuonFfi_MOD
module[16] = module.charToString
module[17] = module.stringRelOp
module[18] = module.setSub
module[19] = bit.band
module[20] = module.setDiv
module[21] = module.setTest
module[22] = setmetatable
module[23] = module.is_a
module[25] = module.println
module[26] = ffi.sizeof -- bytesize
module[27] = module.strcpy
module[28] = TRAP
module[29] = ASSERT
module[30] = module.removeElemFromSet
module[31] = math.ldexp
module[32] = module.UNPACK
module[33] = module.ODD
module[34] = math.abs
module[35] = bit.lshift
module[36] = bit.arshift
module[37] = bit.ror
module[38] = math.floor
module[39] = module.bool_to_number
module[40] = getmetatable
module[41] = bit.bxor
module[42] = ADDRESSOF
module[43] = module.createBoolArray
module[44] = ABORT
module[45] = C.LuonFfi_DBGTRACE -- or DBGTRACE
module[46] = C.LuonFfi_CRASH
module[47] = C.LuonFfi_TRACE -- or TRACE
module[48] = C.LuonFfi_NOP
module[49] = ffi.new
module[50] = ffi.copy
module[51] = module.min_size
module[52] = jit.off
module[53] = module.ldmod
module[54] = module.ldcmd
module[55] = string.char

return module

