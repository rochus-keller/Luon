local io = require("io")
local string = require("string")
local ffi = require("ffi")


-- File = RECORD END;
local FileMeta = {}

-- Open (name: ARRAY OF CHAR): File EXTERN;
function Files_Open(name)
	local f = {}
	setmetatable(f,FileMeta)
	f.h = io.open(name,"rw")
	if h == nil then return nil end
	return f
end

-- Close (f: File) EXTERN;
function Files_Close(f)
	if f.h ~= nil then 
		f.h:close()
		f.h = nil
	end
end

-- Delete (name: ARRAY OF CHAR):BOOLEAN EXTERN;
function Files_Delete(name)
	-- TODO
end

-- Rename (old, new: ARRAY OF CHAR;res: INTEGER):BOOLEAN EXTERN;
function Files_Rename(old, new)
	-- TODO
end

-- Length (f: File): INTEGER EXTERN;
function Files_Length(f)
	if f.h == nil then return end
	local pos = f.h:seek("cur")
	f.h:seek("end")
	local len = f.h:seek("cur")
	f.h:seek("set", pos)
end

-- Seek (f: File; pos: INTEGER):BOOLEAN EXTERN;
function Files_Seek(f, pos)
	if f.h == nil then return end
	f.h:seek("set", pos)
	return f.h:seek("cur") == pos
end

-- Pos (f: File): INTEGER EXTERN;
function Files_Pos(f)
	if f.h == nil then return end
	return f.h:seek("cur")
end

-- Read (f: File; VAR x: CHAR) EXTERN;
function Files_Read(f)
	if f.h == nil then return 0,0 end
	local str = f.h:read(1)
	return 0, string.byte(str,1)
end

-- ReadBytes (f: File; x: ARRAY OF CHAR; n: INTEGER):INTEGER EXTERN;
function Files_ReadBytes(f, x, n)
	if f.h == nil then return end
	local bytes = f.h:read(n)
	ffi.copy(x, bytes, n)
	return #bytes
end

-- Write (f: File; x: CHAR) EXTERN;
function Files_Write(f, x)
	if f.h == nil then return end
	f.h:write(string.char(x))
end

-- WriteBytes (f: File; x: ARRAY OF CHAR) EXTERN;
function Files_WriteBytes(f, x, n)
	if f.h == nil then return end
	f.write(x)
end	
	
