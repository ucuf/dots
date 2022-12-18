local gears = require('gears')
local watch = require("awful.widget.watch")
local wibox = require("wibox")

local WIDGET_DIR = gears.filesystem.get_configuration_dir() .. '/widgets'
local ICONS_DIR  = WIDGET_DIR .. '/icons'

local net_speed_widget = {}

local function bytes_to_h(bytes)
	local base = 1024
    local speed
    local dim

    if bytes < base then
        speed = bytes
        dim = 'B'
    elseif bytes < base * base then
        speed = bytes / base
        dim = 'KiB'
    elseif bytes < base * base * base then
        speed = bytes / (base * base)
        dim = 'MiB'
    elseif bytes < base * base * base * base then
        speed = bytes / (base * base * base)
        dim = 'GiB'
    else
        speed = tonumber(bytes)
        dim = 'B'
    end

	return string.format("%.1f %s", speed, dim)
end


local function split(string_to_split, separator)
    if separator == nil then separator = "%s" end
    local t = {}

    for str in string.gmatch(string_to_split, "([^".. separator .."]+)") do
        table.insert(t, str)
    end

    return t
end

local function worker(user_args)
	local args = user_args or {}

    local interface = args.interface or ''
    local timeout = args.timeout or 1
    -- local width = args.width or 60

    net_speed_widget = wibox.widget {
		layout = wibox.layout.fixed.horizontal,
		{
			id = 'rx_speed',
			-- forced_width = width,
            align = 'right',
            widget = wibox.widget.textbox
        },
        {
            image = ICONS_DIR .. '/down.svg',
            widget = wibox.widget.imagebox
        },
        {
            id = 'tx_speed',
            -- forced_width = width,
            align = 'left',
            widget = wibox.widget.textbox
        },
        {
            image =  ICONS_DIR .. '/up.svg',
            widget = wibox.widget.imagebox
        },
        set_rx_text = function(self, new_rx_speed)
            self:get_children_by_id('rx_speed')[1]:set_text(tostring(new_rx_speed))
        end,
        set_tx_text = function(self, new_tx_speed)
            self:get_children_by_id('tx_speed')[1]:set_text(tostring(new_tx_speed))
        end
    }

    -- make sure these are not shared across different worker/widgets (e.g. two monitors)
    -- otherwise the speed will be randomly split among the worker in each monitor
    local prev_rx = 0
    local prev_tx = 0

    watch(
		-- string.format([[bash -c "cat /sys/class/net/%s/statistics/*_bytes"]], interface),
		string.format("cat /sys/class/net/%s/statistics/rx_bytes /sys/class/net/%s/statistics/tx_bytes", interface, interface),
		timeout,
		function(widget, stdout)
			local cur_vals = split(stdout, '\r\n')

			local cur_rx = 0
			local cur_tx = 0

			for i, v in ipairs(cur_vals) do
				if i % 2 == 0 then
					cur_tx = cur_tx + v
				else
					cur_rx = cur_rx + v
				end
			end

			local speed_rx = (cur_rx - prev_rx) / timeout
			local speed_tx = (cur_tx - prev_tx) / timeout

			widget:set_rx_text(bytes_to_h(speed_rx))
			widget:set_tx_text(bytes_to_h(speed_tx))

			prev_rx = cur_rx
			prev_tx = cur_tx
		end,
		net_speed_widget
	)

    return net_speed_widget

end

return setmetatable(net_speed_widget, { __call = function(_, ...) return worker(...) end })
