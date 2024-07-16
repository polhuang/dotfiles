--[[

     Steamburn Awesome WM theme 3.0
     github.com/lcpz

--]]

local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local volume_widget                             = require('awesome-wm-widgets.pactl-widget.volume')

local os = os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

local theme                                     = {}
theme.zenburn_dir                               = require("awful.util").get_themes_dir() .. "zenburn"
theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/steamburn"
theme.wallpaper                                 = theme.dir .. "/dusk.png"
theme.font                                      = "Noto Sans 10"
theme.fg_normal                                 = "#e2ccb0"
theme.fg_focus                                  = "#d88166"
theme.fg_urgent                                 = "#CC9393"
theme.bg_normal                                 = "#140c0b"
theme.bg_focus                                  = "#140c0b"
theme.bg_urgent                                 = "#2a1f1e"
theme.border_width                              = dpi(2)
theme.border_normal                             = "#302627"
theme.border_focus                              = "#c2745b"
theme.border_marked                             = "#CC9393"
theme.taglist_fg_focus                          = "#d88166"
theme.tasklist_bg_focus                         = "#140c0b"
theme.tasklist_fg_focus                         = "#d88166"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.menu_height                               = dpi(16)
theme.menu_width                                = dpi(140)
theme.awesome_icon                              = theme.dir .."/icons/awesome.png"
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.layout_txt_tile                           = "[tile]"
theme.layout_txt_tileleft                       = "[tileleft]"
theme.layout_txt_tilebottom                     = "[tilebottom]"
theme.layout_txt_tiletop                        = "[tiletop]"
theme.layout_txt_fairv                          = "[fairv]"
theme.layout_txt_fairh                          = "[fairh]"
theme.layout_txt_spiral                         = "[spiral]"
theme.layout_txt_dwindle                        = "[dwindle]"
theme.layout_txt_max                            = "[max]"
theme.layout_txt_fullscreen                     = "[full]"
theme.layout_txt_magnifier                      = "[magnified]"
theme.layout_txt_floating                       = "[floating]"
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = dpi(0)
theme.titlebar_close_button_normal              = theme.zenburn_dir.."/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.zenburn_dir.."/titlebar/close_focus.png"
theme.titlebar_minimize_button_normal           = theme.zenburn_dir.."/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.zenburn_dir.."/titlebar/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme.zenburn_dir.."/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.zenburn_dir.."/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.zenburn_dir.."/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.zenburn_dir.."/titlebar/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.zenburn_dir.."/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.zenburn_dir.."/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.zenburn_dir.."/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.zenburn_dir.."/titlebar/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.zenburn_dir.."/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.zenburn_dir.."/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.zenburn_dir.."/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.zenburn_dir.."/titlebar/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.zenburn_dir.."/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.zenburn_dir.."/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.zenburn_dir.."/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.zenburn_dir .. "/titlebar/maximized_focus_active.png"

-- lain related
theme.layout_txt_termfair                       = "[termfair]"
theme.layout_txt_centerfair                     = "[centerfair]"

local markup = lain.util.markup
local gray   = "#94928F"

-- Textclock
local mytextclock = wibox.widget.textclock(markup(gray, " 時間 ") .. "%H:%M ")
mytextclock.font = "IBM Plex Mono 10"

-- Calendar
theme.cal = lain.widget.cal({
    attach_to = { mytextclock },
    notification_preset = {
        font = "IBM Plex Mono",
        fg   = theme.fg_normal,
        bg   = theme.bg_normal
    }
})

-- MPD
-- theme.mpd = lain.widget.mpd({
--     settings = function()
--         artist = mpd_now.artist .. " "
--         title  = mpd_now.title  .. " "

--         if mpd_now.state == "pause" then
--             artist = "mpd "
--             title  = "paused "
--         elseif mpd_now.state == "stop" then
--             artist = ""
--             title  = ""
--         end

--         widget:set_markup(markup.font(theme.font, markup(gray, artist) .. title))
--     end
-- })

-- CPU
local cpu = lain.widget.cpu({
    settings = function()
       widget:set_markup(markup.font("IBM Plex Mono 10", markup(gray, " CPU ") .. cpu_now.usage .. "% "))
    end
})

-- Coretemp
local temp = lain.widget.temp({
    settings = function()
       widget:set_markup(markup.font("IBM Plex Mono 10", markup(gray, " 温度 ") .. coretemp_now .. "°C "))
    end
})

-- MEM
local mem = lain.widget.mem({
    settings = function()
      widget:set_markup(markup.font("IBM Plex Mono 10", markup(gray, " RAM ") .. mem_now.perc .. "% "))
   end
})

-- /home fs
--[[ commented because it needs Gio/Glib >= 2.54
theme.fs = lain.widget.fs({
    partition = "/home",
    notification_preset = { fg = theme.fg_normal, bg = theme.bg_normal, font = "Terminus 10.5" },
})
--]]

-- Battery
local bat = lain.widget.bat({
    settings = function()
        local perc = bat_now.perc
        if bat_now.ac_status == 1 then perc = perc .. " Plug" end
        widget:set_markup(markup.font("IBM Plex Mono 10", markup(gray, "  at ") .. perc .. " "))
    end
})

-- Net checker
-- local net = lain.widget.net({
--     settings = function()
--         if net_now.state == "up" then net_state = "On"
--         else net_state = "Off" end
--         widget:set_markup(markup.font(theme.font, markup(gray, " Net ") .. net_state .. " "))
--     end
-- })

local neticon = wibox.widget.imagebox(theme.widget_net)
local net = lain.widget.net({
    settings = function()
       widget:set_markup(markup.font("IBM Plex Mono 10", markup(gray, "   入")) ..
                         markup.font("IBM Plex Mono 10", markup("#7AC82E", " " .. string.format("%06.0f", net_now.received)))
                         .. markup.font("IBM Plex Mono 10", markup.font("IBM Plex Mono 10", markup(gray, "  出"))) ..
                         markup.font("IBM Plex Mono 10", markup("#46A8C3", " " .. string.format("%06.0f", net_now.sent) .. " ")))
    end
})

-- ALSA volume
-- local volicon = wibox.widget.imagebox(theme.widget_vol)
-- theme.volume = lain.widget.pulse({
--     settings = function()
--         vlevel = volume_now.left .. "-" .. volume_now.right .. "% | " .. volume_now.device
--         if volume_now.muted == "yes" then
--             vlevel = vlevel .. " M"
--         end
--         widget:set_markup(lain.util.markup("#7493d2", vlevel))
--     end
-- })

-- theme.volume.widget:buttons(awful.util.table.join(
--     awful.button({}, 1, function() -- left click
--         awful.spawn("pavucontrol-qt")
--     end),
--     awful.button({}, 4, function() -- scroll up
--         os.execute(string.format("pactl set-sink-volume %s +1%%", volume.device))
--         volume.update()
--     end),
--     awful.button({}, 5, function() -- scroll down
--         os.execute(string.format("pactl set-sink-volume %s -1%%", volume.device))
--         volume.update()
--     end)
-- ))

-- Weather
--[[ to be set before use
theme.weather = lain.widget.weather({
    --APPID =
    city_id = 2643743, -- placeholder (London)
})
--]]

-- Separators
local first = wibox.widget.textbox("  ")
local spr   = wibox.widget.textbox("  ")

local function update_txt_layoutbox(s)
    -- Writes a string representation of the current layout in a textbox widget
   local txt_l = theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))] or "[default]"
   s.mytxtlayoutbox:set_text(txt_l .. "  ")
   s.mytxtlayoutbox.font = "IBM Plex Mono 8"
end

function theme.at_screen_connect(s)
    -- Quake application
    s.quake = lain.util.quake({ app = awful.util.terminal })

    -- If wallpaper is a function, call it with the screen
    local wallpaper = theme.wallpaper
    if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)

    -- Tags
    awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Textual layoutbox
    s.mytxtlayoutbox = wibox.widget.textbox(theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))])
    awful.tag.attached_connect_signal(s, "property::selected", function () update_txt_layoutbox(s) end)
    awful.tag.attached_connect_signal(s, "property::layout", function () update_txt_layoutbox(s) end)
    s.mytxtlayoutbox:buttons(my_table.join(
                           awful.button({}, 1, function() awful.layout.inc(1) end),
                           awful.button({}, 2, function() awful.layout.set( awful.layout.layouts[1] ) end),
                           awful.button({}, 3, function() awful.layout.inc(-1) end),
                           awful.button({}, 4, function() awful.layout.inc(1) end),
                           awful.button({}, 5, function() awful.layout.inc(-1) end)))
   

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

    s.mytaglist = awful.widget.taglist{
       screen = s,
       filter = awful.widget.taglist.filter.all,
       buttons = awful.util.taglist_buttons,
       style = {
          font = "IBM Plex Mono 10"
       },
       widget_template = {
          {
             {
                {
                   id = 'text_role',
                   widget = wibox.widget.textbox,
                },
                left = 15,
                right = 15,
                widget = wibox.container.margin,
             },
             id = 'background_role',
             widget = wibox.container.background,
          },
          layout = wibox.layout.fixed.horizontal,
       },
    }
    
    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

    s.mytasklist = awful.widget.tasklist{
       screen = s,
       filter = awful.widget.tasklist.filter.currenttags,
       style = {
          font = "IBM Plex Mono 8"
       }
    }
    
    s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(25) })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            first,
            s.mytaglist,
            spr,
            s.mytxtlayoutbox,
            --spr,
            s.mypromptbox,
            spr,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
           layout = wibox.layout.fixed.horizontal,
           volume_widget{
              width=100,
              widget_type="horizontal_bar"
	    },
           net.widget,
            --theme.mail.widget,
           cpu.widget,
           temp.widget,
           mem.widget,
           --bat.widget,
           mytextclock
        },
    }
end

return theme
