local module = {}
local clock = os.clock

function module.Available()
	return 0
end

function module.Read() -- (VAR ch: CHAR)
	return 0, 0
end

function module.Mouse() -- (VAR keys: SET; VAR x, y: INTEGER)
	return 0, 0, 0, 0
end

function module.SetMouseLimits() -- (w, h: INTEGER)
end

function module.Time()
    return clock() * 1000.0 * 1000.0
end


-- NOTE: these numbers are allocated as by LjbcGen and need to be updated if Input.luon changes!
module[0] = 1000000
module[1] = module.Available
module[2] = module.Read
module[3] = module.Mouse
module[4] = module.SetMouseLimits
module[5] = module.Time

return module
