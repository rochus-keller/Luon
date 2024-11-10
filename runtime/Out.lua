local tochar = require("string").char
local ffi = require("ffi")

function Out_Open()
end

function Out_Char(ch)
    if type(ch) == "number" then
        io.stdout:write(tochar(ch))
    else
        io.stdout:write(tostring(ch))
    end
end

function Out_String(s)
	if type(s) == "cdata" then
		io.stdout:write(ffi.string(s))
	else
	    io.stdout:write(tostring(s))
	end
end

function Out_Int(i,n)
        io.stdout:write(string.format("%"..tostring(n).."d",i))
        -- io.stdout:write(string.format("%d",i) )
end

function Out_Real(x,n)
    -- io.stdout:write(x) -- effect of n not properly specified
    io.stdout:write(string.format("%"..tostring(n).."e",x))
end

function Out_Ln()
    io.stdout:write("\n")
end

