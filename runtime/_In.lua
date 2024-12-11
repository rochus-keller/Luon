

function In_Arguments()
	if arg then
		local args = { count = #arg }
		for i=1,#arg do
			args[i-1] = arg[i]
		end
		return args
	else
		return nil
	end
end
