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

local module = { ["@mod"] = "LUON" }
LUON = module -- directly publish the module by global var

ffi.cdef[[
	typedef uint8_t CharArray[?];
]]

local CharArray = ffi.typeof("CharArray")
local bytesize = ffi.sizeof
local frexp = math.frexp

function module.stringToCharArray(str, len)
        if str == nil then return nil end
        local n = #str
        if len == nil or len < n+1 then len = n+1 end
        local a = ffi.new( CharArray, len )
        ffi.copy(a, str)
	return a
end
function module.createLuaArray(len)
	local a = { count = len }
	return a
end
function module.createCharArray(len)
        return ffi.new( CharArray, len )
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
        if type(str) == "string" then
            return #str
        end
        if not ffi.istype(CharArray,str) then return 0 end

        local i = 0
        while str[i] ~= 0 do i = i + 1 end
        return i
end
function module.arraylen( array )
    if ffi.istype(CharArray,array) then
         return bytesize(array)
    end
    local t = type(array)
    if t  == "table" then
        return array.count
    elseif t == "string" then
        return #array + 1
    else
        return 0
    end
end
function module.charArrayToString(array)
    if ffi.istype(CharArray,array) then
        return ffi.string(array)
    else
        return tostring(array)
    end
end
function module.stringRelOp( lhs, rhs, op )
    local res = false
    if op == 1 then return lhs == rhs end -- EQ
    if op == 2 then return lhs ~= rhs end -- NEQ
    if op == 3 then return lhs < rhs end -- LT
    if op == 4 then return lhs <= rhs end -- LEQ
    if op == 5 then return lhs > rhs end -- GT
    if op == 6 then return lhs >= rhs end -- GEQ
    return false;
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
local function containsNonPrintable(str)
    -- Pattern to match non-printable characters (excluding \0, but including tabs and newlines)
    local pattern = "[\x01-\x1F\x7F-\xFF]"
    return str:match(pattern) ~= nil
end
local function toHex(str)
    local hexStr = ""
    for i = 1, #str do
        local byte = string.format("%02x", str:byte(i, i))
        hexStr = hexStr .. byte
    end
    return hexStr
end
function module.println( val )
        module.print(val)
        io.stdout:write("\n")
end
function module.tostring(val)
    local str
    if ffi.istype(CharArray,val) then
        str = ffi.string(val)
    else
        str = tostring(val)
    end
    if containsNonPrintable(str) then
        str = "$"..toHex(str).."$"
    end
    return str
end
function module.print( val )
    io.stdout:write(module.tostring(val))
end
function module.ODD(num)
	return ( num % 2 ) == 1
end
function module.DIV(a,b)
    return math.floor(a/b)
end
function module.MOD(a,b)
    return a % b
end
function module.clone(obj, fieldcount)
    if obj == nil then error("object is nil") end
    local res
    if ffi.istype(CharArray,obj) then
        local n = bytesize(obj)
        res = ffi.new(CharArray, n)
        ffi.copy(res,obj,n)
    elseif fieldcount ~= nil then
        res = {}
        setmetatable(res,getmetatable(obj))
        for i=0,fieldcount do
            res[i] = obj[i]
        end
    elseif obj.count then
        res = {}
        res.count = obj.count
        for i=0,obj.count do
            res[i] = obj[i]
        end
    elseif type(obj) == "string" then
        return module.stringToCharArray(obj)
    else
        res = {}
        for k,v in pairs(obj) do
            res[k] = v
        end
    end
    return res
end

if ASSERT == nil then
    function ASSERT(cond,line,file)
        if cond then return end
        error("ASSERT called in "..tostring(file).." on line "..tostring(line))
    end
end

function module.assureNotNil(obj, txt)
    if obj == nil then error(txt) end
end

function module.require(name)
    local m                   -- we cannot use require(name) here because it doesn't allow circular loads
    m = package.loaded[name]
    if m then return m end
    local f = package.preload[name]
    if f then m = f(name) end
    package.loaded[name] = m
    if m == nil then return m end
    f = m["$begin"]
    if f then f() end
    return m
end

LUON_require = module.require

-- Magic mumbers used by the compiler
module[6] = module.assureNotNil
module[7] = module.stringToCharArray
module[8] = module.createCharArray
module[9] = addElemToSet
module[10] = module.addRangeToSet
module[11] = bit.bnot
module[12] = bit.bor
module[14] = module.DIV
module[15] = module.MOD
module[17] = module.stringRelOp
module[18] = module.setSub
module[19] = bit.band
module[20] = module.setDiv
module[21] = module.setTest
module[22] = setmetatable
module[23] = module.is_a
module[24] = strlen
module[25] = module.println
module[26] = ffi.sizeof -- bytesize
module[28] = TRAP
module[29] = ASSERT
module[30] = module.removeElemFromSet
module[33] = module.ODD
module[34] = math.abs
module[35] = bit.lshift
module[36] = bit.arshift
module[37] = bit.ror
module[38] = math.floor
module[40] = getmetatable
module[41] = bit.bxor
module[52] = jit.off
module[55] = string.char
module[56] = module.print
module[57] = bit.rshift
module[58] = module.arraylen
module[59] = string.byte
module[60] = module.clone
module[61] = module.charArrayToString
module[62] = module.require
module[63] = module.tostring

return module

