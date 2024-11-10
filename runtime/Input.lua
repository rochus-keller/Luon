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
module[2] = 1000000
module[3] = module.Available
module[4] = module.Read
module[5] = module.Mouse
module[6] = module.SetMouseLimits
module[7] = module.Time

return module
