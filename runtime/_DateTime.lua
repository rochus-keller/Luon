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

local os = require("os")

local DateTimeMeta = {}


function DateTime_current()
	local dt = {}
	setmetatable(dt,DateTimeMeta)
	local tmp = os.date ("*t")
	dt[0] = tmp.year
	dt[1] = tmp.month
	dt[2] = tmp.day
	dt[3] = tmp.hour
	dt[4] = tmp.min
	dt[5] = tmp.sec
	return dt
end

function DateTime_diff(from, to)
	local a = os.time({ year = from[0], month = from[1], day = from[2], 
							hour = from[3], min = from[4], sec = from [5]})
	local b = os.time({ year = to[0], month = to[1], day = to[2], 
							hour = to[3], min = to[4], sec = to [5]})
	return os.difftime(b, a)
end
