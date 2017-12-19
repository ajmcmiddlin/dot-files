#!/bin/sh
mode="1920x1200_59.95"
xrandr --rmmode $mode
xrandr --newmode  $mode 192.99  1920 2048 2256 2592  1200 1201 1204 1242  -HSync +Vsync
xrandr --addmode DP1-2 $mode
xrandr --output VIRTUAL1 --off --output eDP1 --off --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output DP1-3 --off --output DP1-1 --off --output DP1-2 --primary --mode $mode --pos 0x0 --rotate normal --output DP2 --off

