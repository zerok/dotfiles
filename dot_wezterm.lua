local wezterm = require 'wezterm'
local config = {}
config.color_scheme = 'Dracula'
config.font_size = 13.0
config.font = wezterm.font('JetBrains Mono')
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.send_composed_key_when_left_alt_is_pressed = true
return config
