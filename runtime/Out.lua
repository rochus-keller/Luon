local tochar = require("string").char
local ffi = require("ffi")
local luon = require("LUON")
local module = {}

-- just a quick first implementation

function module.Open()
end

function module.Char(ch)
    if type(ch) == "number" then
        io.stdout:write(tochar(ch))
    else
        io.stdout:write(tostring(ch))
    end
end

function module.String(s)
	if type(s) == "cdata" then
		io.stdout:write(ffi.string(s))
	else
	    io.stdout:write(tostring(s))
	end
end

function module.Int(i,n)
        io.stdout:write(string.format("%"..tostring(n).."d",i))
        -- io.stdout:write(string.format("%d",i) )
end

function module.Real(x,n)
    -- io.stdout:write(x) -- effect of n not properly specified
    io.stdout:write(string.format("%"..tostring(n).."e",x))
end

function module.Ln()
    io.stdout:write("\n")
end

-- NOTE: these numbers are as allocated by LjbcGen and need to be updated if Out.luon changes!
module[2] = module.Open
module[3] = module.Char
module[4] = module.String
module[5] = module.Int
module[6] = module.Real
module[7] = module.Ln

return module
