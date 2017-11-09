#!/bin/sh
mode="laptopOnly"
xrandr --rmmode $mode
xrandr --newmode  $mode 197.97  2048 2184 2408 2768  1152 1153 1156 1192  -HSync +Vsync
xrandr --addmode eDP1 laptopOnly
xrandr --output VIRTUAL1 --off --output eDP1 --mode $mode --pos 0x0 --rotate normal --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output DP2 --off
