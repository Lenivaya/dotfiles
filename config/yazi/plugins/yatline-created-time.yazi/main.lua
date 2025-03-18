function hovered()
	local hovered = cx.active.current.hovered
	if hovered then
		return hovered
	else
		return ""
	end
end

local function setup(_, options)
	options = options or {}

	local config = {
		created_time_color = options.created_time_color or "silver",
	}

	if Yatline ~= nil then
		function Yatline.coloreds.get:created_time()
			local h = hovered()
			local created_time = {}
			local time = " C: " .. os.date("%Y-%m-%d %H:%M", h.cha.btime // 1) .. " "

			table.insert(created_time, { time, config.created_time_color })
			return created_time
		end
	end
end

return { setup = setup }
