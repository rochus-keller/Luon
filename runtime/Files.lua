local io = require("io")
local string = require("string")
local ffi = require("ffi")

local module = {}


-- File = RECORD END;
local FileMeta = {}

-- Open (name: ARRAY OF CHAR): File EXTERN;
function module.Open(name)
	local f = {}
	setmetatable(f,FileMeta)
	f.h = io.open(name,"rw")
	if h == nil then return nil end
	return f
end

-- Close (f: File) EXTERN;
function module.Close(f)
	if f.h ~= nil then 
		f.h:close()
		f.h = nil
	end
end

-- Delete (name: ARRAY OF CHAR):BOOLEAN EXTERN;
function module.Delete(name)
	-- TODO
end

-- Rename (old, new: ARRAY OF CHAR;res: INTEGER):BOOLEAN EXTERN;
function module.Rename(old, new)
	-- TODO
end

-- Length (f: File): INTEGER EXTERN;
function module.Length(f)
	if f.h == nil then return end
	local pos = f.h:seek("cur")
	f.h:seek("end")
	local len = f.h:seek("cur")
	f.h:seek("set", pos)
end

-- Seek (f: File; pos: INTEGER):BOOLEAN EXTERN;
function module.Seek(f, pos)
	if f.h == nil then return end
	f.h:seek("set", pos)
	return f.h:seek("cur") == pos
end

-- Pos (f: File): INTEGER EXTERN;
function module.Pos(f)
	if f.h == nil then return end
	return f.h:seek("cur")
end

-- Read (f: File; VAR x: CHAR) EXTERN;
function module.Read(f)
	if f.h == nil then return 0,0 end
	local str = f.h:read(1)
	return 0, string.byte(str,1)
end

-- ReadBytes (f: File; x: ARRAY OF CHAR; n: INTEGER):INTEGER EXTERN;
function module.ReadBytes(f, x, n)
	if f.h == nil then return end
	local bytes = f.h:read(n)
	ffi.copy(x, bytes, n)
	return #bytes
end

-- Write (f: File; x: CHAR) EXTERN;
function module.Write(f, x)
	if f.h == nil then return end
	f.h:write(string.char(x))
end

-- WriteBytes (f: File; x: ARRAY OF CHAR) EXTERN;
function module.WriteBytes(f, x, n)
	if f.h == nil then return end
	f.write(x)
end	
	
-- NOTE: these numbers are allocated as by LjbcGen and need to be updated if Input.luon changes!
		
module[0] = FileMeta -- File = RECORD END;
module[1] = module.Open -- Open (name: ARRAY OF CHAR): File EXTERN;
module[2] = module.Close -- Close (f: File) EXTERN;
module[3] = module.Delete -- Delete (name: ARRAY OF CHAR):BOOLEAN EXTERN;
module[4] = module.Rename -- Rename (old, new: ARRAY OF CHAR):BOOLEAN EXTERN;
module[5] = module.Length -- Length (f: File): INTEGER EXTERN;
module[6] = module.Seek -- Seek (f: File; pos: INTEGER):BOOLEAN EXTERN;
module[7] = module.Pos -- Pos (f: File): INTEGER EXTERN;
module[8] = module.Read -- Read (f: File; VAR x: CHAR) EXTERN;
module[9] = module.ReadBytes -- ReadBytes (f: File; x: ARRAY OF CHAR; n: INTEGER) EXTERN;
module[10] = module.Write -- Write (f: File; x: CHAR) EXTERN;
module[11] = module.WriteBytes -- WriteBytes (f: File; x: ARRAY OF CHAR;n: INTEGER) EXTERN;

return module
