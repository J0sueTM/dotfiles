#!/usr/bin/env bash
# Lay out displays (external above the laptop panel, both at their highest
# refresh rate), then paint the wallpaper and (re)start polybar.
# Run by i3 on every start/reload, so `mod+shift+c` after plugging a monitor
# is enough.

set -u

dotfiles=$(cd "$(dirname "$(readlink -f "$0")")/../.." && pwd)

# prints "<preferred mode> <highest rate for it>" for an output
best_mode() {
  xrandr --query | awk -v o="$1" '
    $1 == o { found = 1; next }
    found && /^[^ ]/ { exit }
    found {
      rate = 0
      for (i = 2; i <= NF; i++) { v = $i; gsub(/[*+]/, "", v); if (v + 0 > rate) rate = v + 0 }
      print $1, rate
      exit
    }'
}

internal=$(xrandr --query | awk '/^eDP/ { print $1; exit }')
external=$(xrandr --query | awk -v i="$internal" '/ connected/ && $1 != i { print $1; exit }')

# drop outputs that were unplugged but still hold a mode
for out in $(xrandr --query | awk '/ disconnected [0-9]/ { print $1 }'); do
  xrandr --output "$out" --off
done

if [[ -n $internal ]]; then
  read -r mode rate < <(best_mode "$internal")
  xrandr --output "$internal" --mode "$mode" --rate "$rate" --pos 0x0
fi

if [[ -n $external ]]; then
  read -r mode rate < <(best_mode "$external")
  if [[ -n $internal ]]; then
    xrandr --output "$external" --mode "$mode" --rate "$rate" --primary --above "$internal"
  else
    xrandr --output "$external" --mode "$mode" --rate "$rate" --primary
  fi
elif [[ -n $internal ]]; then
  xrandr --output "$internal" --primary
fi

feh --no-fehbg --bg-fill "$dotfiles/wallpaper/mountains.jpg"

~/.config/polybar/launch.sh
