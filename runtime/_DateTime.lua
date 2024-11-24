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
