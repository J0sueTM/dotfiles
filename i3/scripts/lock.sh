#!/usr/bin/env bash
# Lock with a pixelated, darkened shot of the screen. Used by xss-lock, so it
# must block (--nofork) until unlocked.

img=${XDG_RUNTIME_DIR:-/tmp}/lock.png
im=$(command -v magick || command -v convert)

args=(--nofork --ignore-empty-password --show-failed-attempts --color=0b0e0d)
if maim --hidecursor "$img" 2>/dev/null && [[ -n $im ]] &&
  "$im" "$img" -scale 4% -scale 2500% -fill '#0b0e0d' -colorize 45% "$img"; then
  args+=(--image="$img")
fi

dunstctl set-paused true 2>/dev/null
i3lock "${args[@]}"
dunstctl set-paused false 2>/dev/null
rm -f "$img"
