{ config, lib, pkgs, ... }:
{
	imports = [
	];
	wayland.windowManager.hyprland = {
		enable = true;
		plugins = [];
		settings = {};
		xwayland = { enable = true; };
		systemd.enable = true;

		extraConfig = ''
			exec-once = swww init
			exec-once = swww img /home/shiva/Pictures/space3.jpg
			exec-once = wl-paste --watch cliphist store
			exec-once = pypr	# https://github.com/hyprland-community/pyprland

			$mainMod = SUPER

			bind = $mainMod_SHIFT	, F	    , exec	     , pypr toggle term && hyprctl dispatch bringactivetop
			bind = $mainMod		, SPACE     , exec           , rofi -show drun
			bind = $mainMod         , return    , exec           , kitty

			bind = $mainMod_SHIFT   , return    , exec           , alacritty --class floating
			bind = $mainMod         , Q         , killactive     ,
			     bind = $mainMod         , E         , exec           , thunar
				     bind = $mainMod         , F         , fullscreen
				     bind = $mainMod_SHIFT   , B         , exec           , brave --class floating
				     bind = $mainMod         , B         , exec           , brave
				     bind = $mainMod         , C         , exec           , code
				     bind = $mainMod         , T         , togglefloating ,
			     bind = $mainMod         , Y         , togglesplit    , # dwindle
				     bind = SHIFT		, Print	    , exec	     , grimshot --notify save area
				     bind = $mainMod_SHIFT   , R         , exec           , ~/.config/waybar/launch.sh
				     bind = CONTROL ALT	, Q	    , exec	     , swaylock
				     bind = $mainMod		, code:60   , exec	     , rofi -modi emoji -show emoji -config  ~/.config/rofi/launchers/type-1/style-6.rasi
				     bind = $mainMod, P, exec, cliphist list | rofi -dmenu | cliphist decode | wl-copy
# W I N D O W - T O G G L E
				     bind = $mainMod, G, togglegroup
				     bind = $mainMod SHIFT, G, lockgroups, toggle
# W I N D O W - P I N
				     bind = $mainMod, p, pin
# N A V I G A T E - in a group
				     bind = $mainMod , code:34, changegroupactive, b	#win+shift+[
				     bind = $mainMod , code:35, changegroupactive, f	#win+shift+]

# Move focus with mainMod + arrow keys
					     bind = $mainMod , h , movefocus, l
					     bind = $mainMod , l , movefocus, r
					     bind = $mainMod , k , movefocus, u
					     bind = $mainMod , j , movefocus, d

# Swith to previous workspace
					     bind = $mainMod , TAB , workspace, previous

# Switch workspaces with mainMod + [0-9]
					     bind = $mainMod , 1 , workspace , 1
					     bind = $mainMod , 2 , workspace , 2
					     bind = $mainMod , 3 , workspace , 3
					     bind = $mainMod , 4 , workspace , 4
					     bind = $mainMod , 5 , workspace , 5
					     bind = $mainMod , 6 , workspace , 6
					     bind = $mainMod , 7 , workspace , 7
					     bind = $mainMod , 8 , workspace , 8
					     bind = $mainMod , 9 , workspace , 9
					     bind = $mainMod , 0 , workspace , 10

# Move active window to a workspace with mainMod + SHIFT + [0-9]
					     bind = $mainMod SHIFT , 1 , movetoworkspace , 1
					     bind = $mainMod SHIFT , 2 , movetoworkspace , 2
					     bind = $mainMod SHIFT , 3 , movetoworkspace , 3
					     bind = $mainMod SHIFT , 4 , movetoworkspace , 4
					     bind = $mainMod SHIFT , 5 , movetoworkspace , 5
					     bind = $mainMod SHIFT , 6 , movetoworkspace , 6
					     bind = $mainMod SHIFT , 7 , movetoworkspace , 7
					     bind = $mainMod SHIFT , 8 , movetoworkspace , 8
					     bind = $mainMod SHIFT , 9 , movetoworkspace , 9
					     bind = $mainMod SHIFT , 0 , movetoworkspace , 10

# Move active window in workspace
					     bind = $mainMod SHIFT , h , movewindow, l
					     bind = $mainMod SHIFT , l , movewindow, r
					     bind = $mainMod SHIFT , k , movewindow, u
					     bind = $mainMod SHIFT , j , movewindow, d

# Audiocontrol with mediakeys & show OSD values
					     bind = , XF86AudioRaiseVolume , exec , swayosd --output-volume raise
					     bind = , XF86AudioLowerVolume , exec , swayosd --output-volume lower
					     bind = , XF86AudioMute        , exec , swayosd --output-volume mute-toggle

# Mediacontrol with mediakeys
					     bind = , XF86AudioPlay , exec , playerctl play-pause
					     bind = $mainMod , BackSpace, exec , playerctl play-pause
					     bind = , XF86AudioPrev , exec , playerctl previous
					     bind = , XF86AudioNext , exec , playerctl next
					     bind = $mainMod SHIFT, b, exec , playerctl previous
					     bind = $mainMod SHIFT, n, exec , playerctl next

# Resize windows with mainMod CTRL + arrowkeys
					     bind = $mainMod CONTROL, l, resizeactive , 30 0
					     bind = $mainMod CONTROL, h, resizeactive , -30 0
					     bind = $mainMod CONTROL, k, resizeactive , 0 -30
					     bind = $mainMod CONTROL, j, resizeactive , 0 30

# Move/resize windows with mainMod + LMB/RMB and dragging
					     bindm = $mainMod , mouse:272 , movewindow
					     bindm = $mainMod , mouse:273 , resizewindow

# B R I G H T N E S S
					     binde = , XF86MonBrightnessUp, exec, brightnessctl set +10%
					     binde = , XF86MonBrightnessDown, exec, brightnessctl set 10%-

# Montior
					     monitor = DP-3, 2560x1440@144.0,0x0,1.0


					     general {
						     gaps_in             = 4
							     gaps_out            = 4
							     border_size         = 3
							     col.active_border   = rgba(7287fdff)
#col.active_border   = rgba(89b4faff)
#col.active_border   = rgba(f5c2e7ff)
							     col.inactive_border = rgba(11111bff)
#col.inactive_border = none
							     layout              = dwindle
							     resize_on_border    = false


# G R O U P - B O R D E R S
# col.group_border = none
#col.group_border = rgba(11111bff)
#col.group_border_active = rgba(f5c2e7ff)
#col.group_border_active = none

#col.nogroup_border = rgba(f5c2e7ff)
#col.nogroup_border_active = rgba(f5c2e7ff)

#col.group_border_locked = rgba(f5c2e7ff)
#col.group_border_locked_active = rgba(f5c2e7ff)

#col.group_border = rgba(f51e1e2e)
#col.group_border_active = rgba(f5c2e7ff)
#col.group_border_locked = rgba(f5c2e7ff)
#col.group_border_locked_active = rgba(f5c2e7ff)
					     }

# See https://wiki.hyprland.org/Configuring/Variables/ for more
		decoration {
# rounding
			rounding               = 8

				blur {
					enabled           = true
						size              = 8
						passes            = 2
						noise             = 0
						contrast          = 1
						new_optimizations = true
				}

# shadow
			drop_shadow            = false
				shadow_range           = 4
				shadow_render_power    = 3
				col.shadow             = rgba(1a1a1aee)
		}

# Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
		animations {
			enabled   = false
				bezier    = myBezier    , 0.71 , 0.18 , 1   , 0.09
				bezier    = workspaces  , 1    , 0.25 , 0   , 0.75
				bezier    = angle       , 1    , 1    , 1   , 1

				animation = windows     , 1    , 3    , default
				animation = windowsOut  , 1    , 3    , workspaces , popin
				animation = windowsIn   , 1    , 3    , workspaces , popin
				animation = border      , 1    , 5    , default
				animation = borderangle , 1    , 25   , angle      , 
					  animation = fade        , 1    , 7    , default
						  animation = workspaces  , 1    , 2    , workspaces , slide 
		}

		group {
			col.border_active = rgba(f5c2e7ff)
				col.border_inactive = rgba(11111bff)
				groupbar {
					font_size = 12
						col.active = rgba(f5c2e7ff)
# col.inactive = rgba(f5c2e7ff)
						col.inactive = rgba(11111bff)
# col.locked_active = rgba(11111bff)
# col.locked_inactive = rgba(11111bff)
						render_titles = true
						text_color = rgba(f5c2e7ff)
						gradients = false
				}
		}

# See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
		dwindle {
# master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
			pseudotile     = true 

# col{
#   group_border = 0x66777700
# }
#col.group_border = 0x66777700
#col.group_border_active = 0x66ffff00

#group_border = rgba(FFF000aa)
#groupbar_titles_font_size = 14

# preserve_split = false # you probably want this
				preserve_split = true
				force_split    = 2
		}


# For all categories, see https://wiki.hyprland.org/Configuring/Variables/
		input {
			kb_rules       =
				follow_mouse   = 1

				touchpad {
					natural_scroll = true
				}

			sensitivity    = 0 # -1.0 - 1.0, 0 means no modification.
		}


# See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
		master {
			new_is_master = true
		}

# See https://wiki.hyprland.org/Configuring/Variables/ for more
		gestures {
			workspace_swipe = false
		}

# See https://wiki.hyprland.org/Configuring/Keywords/#per-device-input-configs for more
# device:logitech-mx-master-2s-1 {
#     sensitivity = 50
# }

# See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

		misc {
			vfr                      = true
				vrr                      = 0
				animate_manual_resizes   = false
				focus_on_activate        = false
				render_ahead_of_time     = false
				disable_hyprland_logo    = false


# G R O U P - B O R D E R S
#render_titles_in_groupbar = false
#groupbar_titles_font_size = 12
#groupbar_gradients = false
#groupbar_text_color = rgba(f5c2e7ff)

#group_insert_after_current = false
		}

		debug {
			overlay = false
		}

# Define window behaviour 
		windowrulev2 = float,class:^(pavucontrol)(.*)$
					     windowrulev2 = float,class:^(.blueman-manager)(.*)$
									   windowrulev2 = float,title:^(Picture in picture)(.*)$

												       windowrulev2 = opacity 0.9 0.9, class:^(obsidian)$

# if class is set to floating, then they get float rule
																	       windowrulev2 = float, class:floating

																					   windowrulev2 = float, class:^(scratchpad)$



																									 '';
	};

/*
	home.file."./config/hypr/pyprland.toml".text = ''
	[pyprland]
	plugins = ["scratchpads", "magnify"]

	[scratchpads.term]
	command = "kitty --class scratchpad"
	class = "scratchpad"
	size = "75% 60%"
	max_size = "800px 40%"
	margin = 50
	'';
*/

  	home.file.".config/hypr/pyprland.toml".source = ./pyprland.toml

}

