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

