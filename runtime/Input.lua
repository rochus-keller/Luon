local clock = os.clock

function Input_Available()
	return 0
end

function Input_Read() -- (VAR ch: CHAR)
	return 0, 0
end

function Input_Mouse() -- (VAR keys: SET; VAR x, y: INTEGER)
	return 0, 0, 0, 0
end

function Input_SetMouseLimits() -- (w, h: INTEGER)
end

function Input_Time()
    return clock() * 1000.0 * 1000.0
end


