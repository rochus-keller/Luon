local bit = require("bit")
local ffi = require("ffi")
local C = ffi.C

ffi.cdef[[
	typedef uint8_t CharArray[?];
	typedef uint32_t DisplayBuffer[?];
	
	int PAL_init(unsigned int* b, int l, int w, int h);
	int PAL_deinit();
	int PAL_processEvents(int sleep);
	typedef struct InputState { unsigned int keys; int x; int y; } InputState;
	void PAL_getState(InputState* state);
	char PAL_nextKey();
	int32_t PAL_getTime();
	
	int32_t PAL_listFiles();
	void PAL_fileName(int32_t i, char* name, int len);
	int32_t PAL_openFile(const char* filename);
	int32_t PAL_newFile();
	void PAL_freeFile(int32_t buffer);
	int PAL_saveFile(const char* filename, int32_t buffer);
	int PAL_removeFile(const char* filename);
	int PAL_renameFile(const char* oldName, const char* newName);
	int32_t PAL_length(int32_t buffer);
	int PAL_setPos(int32_t buffer, int32_t pos);
	int32_t PAL_getPos(int32_t buffer);
	int PAL_atEnd(int32_t buffer);
	int PAL_writeByte(int32_t buffer, int32_t byte_);
	int32_t PAL_readByte(int32_t buffer);
]]

local buffer

function PAL_ROR(x,n)
	return bit.ror(x,n)
end

function PAL_LDCMD(module,command) -- :Command 
	local m = LUON_require(ffi.string(module))
	-- print("LDCMD "..ffi.string(module).." module="..tostring(m))
	local cmd
	if m ~= nil then cmd = m[ffi.string(command)] end
	-- print("LDCMD "..ffi.string(command).." command="..tostring(cmd))
	return cmd
end

function PAL_LDMOD(module) -- :boolean 
	local m = LUON_require(ffi.string(module))
	-- print("LDMOD "..ffi.string(module).." res="..tostring(m))
	return m ~= nil
end

function PAL_listFiles() -- : integer 
	return C.PAL_listFiles()
end

function PAL_fileName(i, name, maxlen) 
	C.PAL_fileName(i, name, maxlen) 
end

function PAL_openFile(filename) -- : integer 
	local res = C.PAL_openFile(filename)
	-- print("PAL_openFile "..ffi.string(filename).." res "..tostring(res))
	return res
end

function PAL_newFile() -- : integer 
	return C.PAL_newFile()
end

function PAL_saveFile(filename, buffer) -- : boolean 
	return C.PAL_saveFile(filename, buffer) ~= 0
end

function PAL_freeFile(buffer) 
	C.PAL_freeFile(buffer)
end

function PAL_removeFile(filename) -- : boolean 
	return C.PAL_removeFile(filename) ~= 0
end

function PAL_renameFile(oldName, newName) -- : boolean 
	return C.PAL_renameFile(oldName, newName) ~= 0
end

function PAL_length(buffer) -- : integer 
	return C.PAL_length(buffer)
end

function PAL_setPos(buffer, pos) -- : boolean 
	return C.PAL_setPos(buffer, pos) ~= 0
end

function PAL_atEnd(buffer) -- : boolean 
	return C.PAL_atEnd(buffer) ~= 0
end

function PAL_getPos(buffer) -- : integer 
	return C.PAL_getPos(buffer)
end

function PAL_writeByte(buffer, byte_) -- : boolean 
	return C.PAL_writeByte(buffer, byte_) ~= 0
end

function PAL_readByte(buffer) -- : byte 
	return C.PAL_readByte(buffer)
end

function PAL_nextKey() -- : char 
	return C.PAL_nextKey()
end

-- type InputState* = record keys*: set; x*, y*: integer end

function PAL_getState(state) 
	local s = ffi.new( ffi.typeof("InputState") )
	C.PAL_getState(s)
	state[0] = s.keys
	state[1] = s.x
	state[2] = s.y
end

function PAL_processEvents(sleep) -- : boolean 
	local res = C.PAL_processEvents(sleep) ~= 0
	if res then C.PAL_deinit() end
	return res
end

function PAL_initDisplay(length, w, h) -- : boolean 
	buffer = ffi.new( ffi.typeof("DisplayBuffer"), length )
	return C.PAL_init(buffer, length, w, h) ~= 0
end

function PAL_GETS( a, v ) -- VAR v: SET
	return 0, buffer[a]
end

function PAL_PUTS( a, x ) 
	buffer[a] = x
end

function PAL_getTime() -- : integer 
	return C.PAL_getTime()
end

