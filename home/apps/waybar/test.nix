# Define Waybar
      programs.waybar = let
        # Sway workspaces
        swayWorkspacesModule = {
	  format = "{name}";
          enable-bar-scroll = true;
          warp-on-scroll = false;
          disable-scroll-wraparound = true;
        };
 
	# Sway windows
	swayWindowsModule = {
	  icon = true;
	  icon-size = 15;
	  all-outputs = true;
	  tooltip = false;
	  rewrite = {
	    "(.*) — LibreWolf" = "   $1";
	    "LibreWolf" = "   LibreWolf";
	    "(.*) - YouTube — LibreWolf" = "󰗃   $1";
	  };
	};
 
	# Pipewire/Pulseaudio
	pulseModule = {
	  format = "{icon}  {volume}%";
	  format-bluetooth = "{icon} {volume}%";
	  format-muted = " muted";
	  format-icons = {
	    headphone = "󰋋 ";
	    headset = "󰋋 ";
	    default = [ " " " " ];
	  };
	  on-click = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
	  on-click-middle = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 100%";
	  on-click-right = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 60%";
	  ignored-sinks = [ "Easy Effects Sink" "USB FS AUDIO Analog Stereo" ];
	};
 
	# CPU, Ram and Vram
	cpuModule = {
	  format = "  {usage}%";
	  interval = 3;
	};
	ramModule = {
	  format = "  {used}G";
	  tooltip = false;
	};
	vramModule = {
	  exec = pkgs.writeScript "vramScript" ''
            # Don't run the script if running on integrated graphics
            if lspci -k | grep "Kernel driver in use: nvidia" &> /dev/null || lspci -k | grep "Kernel driver in use: amdgpu" &> /dev/null; then
 
              # Run the nvidia-smi command and capture the VRAM usage and GPU utilization output
              if lspci -k | grep "Kernel driver in use: nvidia" &> /dev/null; then
                vram_usage_mb=$(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits)
                temperature=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits)
              elif lspci -k | grep "Kernel driver in use: amdgpu" &> /dev/null; then
                vram_usage_mb=$(echo "$(cat /sys/class/drm/card0/device/mem_info_vram_used || cat /sys/class/drm/card1/device/mem_info_vram_used) / 1024 / 1024" | bc)
                temperature=$(sensors | grep 'edge' | awk '{print $2}' | sed 's/[^0-9.-]//g')
              fi
 
              # Check if VRAM usage is under 1GB
              if [ $vram_usage_mb -lt 1024 ]; then
                vram_usage_display="$(echo $vram_usage_mb)M"
              else
                # Convert MB to GiB
                vram_usage_gib=$(bc <<< "scale=2; $vram_usage_mb / 1024")
                vram_usage_display="$(echo $vram_usage_gib)G"
              fi
 
              # Print the VRAM usage in MB or GiB, and include GPU utilization and temperature
              echo "{\"text\":\"󰢮  $(echo $vram_usage_display)\",\"tooltip\":\"$(echo $temperature)°C\"}"
 
            else
              :
            fi
	  '';
	  format = "{}";
	  return-type = "json";
	  interval = 3;
	};
 
	# Clocks
	longClockModule = {
	  exec = pkgs.writeScript "longClock" ''
            # Long clock format, with a numeric date and military time tooltip
            time=$(date +'%a %b %d %l:%M:%S%p' | tr -s ' ')
            date=$(date "+%Y-%m-%d")
            echo "{\"text\":\"  $time\",\"tooltip\":\"$date\"}"
	  '';
	  on-click = ''wl-copy $(date "+%Y-%m-%d-%H%M%S"); notify-send "Date copied."'';
	  format = "{}";
	  return-type = "json";
	  interval = 1;
	  tooltip = true;
	};
	shortClockModule = {
	  exec = "echo '  '$(date +'%l:%M%p' | sed 's/^ //')";
	  on-click = ''wl-copy $(date "+%Y-%m-%d-%H%M%S"); notify-send "Date copied."'';
	  interval = 60;
	  tooltip = false;
	};
 
	# Tray, gamemode, bluetooth, and network tray modules
	trayModule = {
	  spacing = 5;
	};
	networkModule = {
	  format-ethernet = "󰈀";
	  format-wifi = "";
	  format-disconnected = "󰖪";
	  format-linked = "";
	  tooltip-format-ethernet = "{ipaddr}\n{ifname} ";
	  tooltip-format-wifi = "{ipaddr}\n{essid} ({signalStrength}%)";
	  tooltip-format-disconnected = "Disconnected";
	};
	bluetoothModule = {
	  format = "";
	  format-disabled = "";
	  format-no-controller = "";
	  tooltip-format-on = "No devices connected.";
	  tooltip-format-connected = "{num_connections} connected\n{device_enumerate}";
	  tooltip-format-enumerate-connected = "{device_alias}";
	  tooltip-format-enumerate-connected-battery = "{device_alias} {device_battery_percentage}%";
	  on-click = "rofi-bluetooth";
	};
	scratchpadModule = {
	  format = "   {count}";
	  show-empty = false;
	  tooltip = true;
	  tooltip-format = "{title}";
	};
	gamemodeModule = {
	  format = "{glyph}";
	  glyph = "󰖺";
	  hide-not-running = true;
	  use-icon = true;
	  icon-spacing = 3;
	  icon-size = 19;
	  tooltip = true;
	  tooltip-format = "Gamemode On";
	};
 
	# Special per-bar modules
	mediaModule = {
	  exec-if = "playerctl --player=mpv status";
	  exec = pkgs.writeScript "mpvMetadata" ''
            get_metadata() {
              playerctl --player=mpv metadata 2>/dev/null |
                awk '/title/{gsub(/\.(mp3|mp4|m4a|mov|flac|opus|oga)$/,""); for (i=3; i<NF; i++) printf $i " "; printf $NF "\n"}'
            }
 
            truncate_string() {
              local str="$1"
              local max_length=30
              if [ $(expr length "$str") -gt $max_length ]; then
                str=$(expr substr "$str" 1 $max_length)...
              fi
              echo "$str"
            }
 
            if playerctl --player=mpv status 2>/dev/null | grep -q Playing; then
              song_name=$(get_metadata | awk -F ' - ' '{print $2}')
              if [ -z "$song_name" ]; then
                song_name=$(get_metadata)
              fi
              echo "{\"text\":\"$(truncate_string "  $song_name")\",\"tooltip\":\"$(get_metadata)\"}"
            elif playerctl --player=mpv status 2>/dev/null | grep -q Paused; then
              artist_name=$(get_metadata | awk -F ' - ' '{print $1}')
              if [ -z "$artist_name" ]; then
                artist_name=$(get_metadata)
              fi
              echo "{\"text\":\"$(truncate_string "  $artist_name")\",\"tooltip\":\"$(get_metadata)\",\"class\":\"paused\"}"
            fi
          '';
	  format = "{}";
	  return-type = "json";
	  interval = 2;
	  max-length = 30;
	  on-click = "playerctl --player=mpv play-pause";
	  on-click-middle = "pkill -9 mpv";
	};
	notificationModule = {
	  exec = pkgs.writeScript "notificationScript" ''
            # Run makoctl mode and store the output in a variable
            mode_output=$(makoctl mode)
 
            # Extract the second line after "default"
            mode_line=$(echo "$mode_output" | sed -n '/default/{n;p}')
 
            # Print the notification status with the tooltip
            if [[ "$mode_line" == "do-not-disturb" ]]; then
              printf '{"text":"󱆥  Off","class":"disabled","tooltip":"Notifications Disabled."}'
            else
              printf '{"text":"  On","tooltip":"Notifications Enabled."}';
            fi
	  '';
	  format = "{}";
	  return-type = "json";
	  interval = 2;
	  on-click = "makotoggle";
	};
	weatherModule = {
	  exec = pkgs.writeScript "weatherScript" ''
            # Define variables
            CITY="Maple"
            API_KEY="18be8db3528f08c33ed9f95698335ea7"
 
            # Fetch weather data
            weather_data=$(curl -s "http://api.openweathermap.org/data/2.5/weather?q=$CITY&appid=$API_KEY")
            weather_condition=$(echo $weather_data | jq -r '.weather[0].main')
 
            # Map weather conditions to emojis
            case "$weather_condition" in
              "Clear") emoji="☀️";;
              "Clouds") emoji="☁️";;
              "Rain") emoji="🌧️";;
              "Drizzle") emoji="🌦️";;
              "Thunderstorm") emoji="⛈️";;
              "Snow") emoji="❄️";;
              "Mist"|"Fog"|"Haze") emoji="🌫️";;
              *) emoji="🌍";; # Default emoji for unknown conditions
            esac
 
            # Extract and format temperature in Celsius
            temperature_kelvin=$(echo $weather_data | jq -r '.main.temp')
            temperature_celsius=$(echo "$temperature_kelvin - 273.15" | bc)
            formatted_temperature=$(printf "%.0f" $temperature_celsius)
 
            # Display weather emoji and temperature
            echo {\"text\":\"$emoji $formatted_temperature°C\",\"tooltip\":\"Weather in $CITY: $weather_condition\"}
	  '';
	  format = "<span font_size='11pt'>{}</span>";
	  return-type = "json";
	  on-click = "xdg-open https://openweathermap.org/city/6173577";
	  interval = 150;
	};
 
	# Laptop modules
	backlightModule = {
	  format = "{icon}  {percent}%";
	  format-icons = ["" "󰖨"];
	  tooltip = false;
	};
	batteryModule = {
	  interval = 60;
	  states = {
	    warning = 30;
	    critical = 15;
	  };
	  format = "{icon}   {capacity}%";
	  format-icons = ["" "" "" "" ""];
	};
      in {
        enable = true;
	settings = {
	  display1 = {
	    name = "bar1";
	    position = "top";
	    layer = "bottom";
	    output = [ display1 "VGA-1" ];
	    modules-left = [ "sway/workspaces" "sway/window" ];
	    modules-right = [ 
	      "pulseaudio" "cpu" "memory" "custom/vram" "custom/clock-long" 
	      "gamemode" "sway/scratchpad" "tray" "bluetooth" "network" 
	    ];
	    "sway/workspaces" = swayWorkspacesModule;
	    "sway/window" = swayWindowsModule;
	    "pulseaudio" = pulseModule;
	    "cpu" = cpuModule;
	    "memory" = ramModule;
	    "custom/vram" = vramModule;
	    "custom/clock-long" = longClockModule;
	    "gamemode" = gamemodeModule;
	    "sway/scratchpad" = scratchpadModule;
	    "tray" = trayModule;
	    "bluetooth" = bluetoothModule;
	    "network" = networkModule // { interface = "enp*"; };
	  };
	  display2 = {
	    name = "bar2";
	    position = "top";
	    layer = "bottom";
	    output = [ display2 ];
	    modules-left = [ "sway/workspaces" "sway/window" ];
	    modules-right = [ 
	      "pulseaudio" "custom/media" "custom/notifs" "cpu" 
	      "memory" "custom/vram" "custom/clock-long" 
	    ];
	    "sway/workspaces" = swayWorkspacesModule;
	    "sway/window" = swayWindowsModule;
	    "pulseaudio" = pulseModule;
	    "custom/media" = mediaModule;
	    "custom/notifs" = notificationModule;
	    "cpu" = cpuModule;
	    "memory" = ramModule;
	    "custom/vram" = vramModule;
	    "custom/clock-long" = longClockModule;
	  };
	  display3 = {
	    name = "bar3";
	    position = "top";
	    layer = "bottom";
	    output = [ display3 ];
	    modules-left = [ "sway/workspaces" "sway/window" ];
	    modules-right = [ 
	      "pulseaudio" "custom/weather" "cpu" "memory" "custom/vram" "custom/clock-short" 
	    ];
	    "sway/workspaces" = swayWorkspacesModule;
	    "sway/window" = swayWindowsModule;
	    "pulseaudio" = pulseModule;
	    "custom/weather" = weatherModule;
	    "cpu" = cpuModule;
	    "memory" = ramModule;
	    "custom/vram" = vramModule;
	    "custom/clock-short" = shortClockModule;
	  };
	  displayLap = {
	    name = "laptop";
	    position = "top";
	    layer = "bottom";
	    output = [ "eDP-1" "LVDS-1" "DSI-1" "HDMI-A-1" ];
	    modules-left = [ "sway/workspaces" "sway/window" ];
	    modules-right = [ 
	      "pulseaudio" "custom/media" "custom/notifs" "custom/weather2" "cpu" "memory" "custom/vram" "backlight" 
	      "battery" "custom/clock-long" "gamemode" "sway/scratchpad" "tray" "bluetooth" "network" 
	    ];
	    "sway/workspaces" = swayWorkspacesModule;
	    "sway/window" = swayWindowsModule;
	    "pulseaudio" = pulseModule;
	    "custom/media" = mediaModule;
	    "custom/notifs" = notificationModule;
	    "custom/weather2" = weatherModule;
	    "cpu" = cpuModule;
	    "memory" = ramModule;
	    "custom/vram" = vramModule;
	    "backlight" = backlightModule;
	    "battery" = batteryModule;
	    "custom/clock-long" = longClockModule;
	    "sway/scratchpad" = scratchpadModule;
	    "tray" = trayModule;
	    "bluetooth" = bluetoothModule;
	    "network" = networkModule;
	  };
	};
	style = ''
          * {
            border: 0;
            border-radius: 0;
            min-height: 0;
            font-family: ${mainFont}, ${nerdFont};
            font-size: 15.5px;
            color: #${textCol};
          }
          #waybar {
            background: #${darkCol};
          }
          #workspaces {
            padding: 0 6px 0 0;
          }
          #tray {
            padding: 0 2px 0 5px;
          }
          #network {
            padding: 0 10px 0 4px;
          }
          #network.disconnected,#bluetooth.off {
            color: #424242;
          }
          #bluetooth {
	    margin: 0 6px 0 4px;
            font-size: 13.4px;
          }
          #workspaces button {
            padding: 0 3px;
            color: white;
            border-bottom: 3px solid transparent;
            min-width: 20px;
          }
          #workspaces button.visible {
            border-bottom: 3px solid #${primeCol};
            background: #${midCol};
          }
          #workspaces button.urgent {
            border-bottom: 3px solid #900000;
          }
          #workspaces button:hover {
            box-shadow: none;
            background: #${lightCol};
          }
          #scratchpad {
            margin-left: 2px;
          }
          #cpu {
            border-bottom: 3px solid #f90000;
	    margin: 0 5px 0 2px;
          }
          #memory {
            border-bottom: 3px solid #4bffdc;
	    margin: 0 5px 0 2px;
          }
          #custom-vram {
            border-bottom: 3px solid #33FF00;
	    margin: 0 5px 0 2px;
          }
          #custom-media {
            border-bottom: 3px solid #ffb066;
	    margin: 0 5px 0 2px;
          }
          #custom-clock-long {
            border-bottom: 3px solid #0a6cf5;
	    margin: 0 5px 0 2px;
          }
          #custom-clock-short {
            border-bottom: 3px solid #0a6cf5;
	    margin: 0 5px 0 2px;
          }
          #backlight {
            border-bottom: 3px solid #5ffca3;
	    margin: 0 5px 0 2px;
          }
          #battery {
            border-bottom: 3px solid #fcfc16;
	    margin: 0 5px 0 2px;
          }
          #custom-media.paused {
            color: #888;
          }
          #custom-weather {
            border-bottom: 3px solid #${primeCol};
	    margin: 0 5px 0 2px;
          }
          #custom-weather2 {
            border-bottom: 3px solid #c75bd3;
	    margin: 0 5px 0 2px;
          }
          #custom-notifs {
            border-bottom: 3px solid #${primeCol};
	    margin: 0 5px 0 2px;
          }
          #custom-notifs.disabled {
            color: #888;
          }
          #pulseaudio {
            margin-right: 5px;
          }
          #pulseaudio.muted {
            color: #424242;
          }
	'';
      };
