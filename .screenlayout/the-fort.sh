#!/bin/sh

# OUTPUT FROM `gtf 1920 1200 60`
# # 1920x1200 @ 60.00 Hz (GTF) hsync: 74.52 kHz; pclk: 193.16 MHz
# Modeline "1920x1200_60.00"  193.16  1920 2048 2256 2592  1200 1201 1204 1242  -HSync +Vsync

output=$1
mode="1920x1200_60"

echo "output is ${output}"

xrandr --rmmode $mode
xrandr --newmode  $mode 193.16  1920 2048 2256 2592  1200 1201 1204 1242  -HSync +Vsync
xrandr --addmode $output $mode

# Turn everything except the laptop display off, then enable the output
# we took as an argument as the primary display, then setup the laptop
# underneath it.
xrandr --output VIRTUAL1 --off \
       --output DP1 --off \
       --output HDMI2 --off \
       --output DP1-2 --off \
       --output DP1-3 --off \
       --output DP1-1 --off \
       --output HDMI1 --off \
       --output DP2 --off \
       --output $output --primary --mode $mode --pos 0x0 --rotate normal \
       --output eDP1 --primary --mode 1280x720 --pos 296x1200 --rotate normal
