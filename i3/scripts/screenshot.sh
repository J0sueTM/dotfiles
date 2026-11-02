#!/usr/bin/env bash
# usage: screenshot.sh region|full
# Copies the image to the clipboard; the file only lives in /tmp.

file=/tmp/screenshot-$(date +%F_%H-%M-%S).png

case ${1:-region} in
  region) maim --hidecursor --select --bordersize=2 --color=0.9,0.71,0.31,1 "$file" ;;
  full) maim --hidecursor "$file" ;;
  *) echo "usage: $0 region|full" >&2; exit 2 ;;
esac || exit 0 # selection cancelled

xclip -selection clipboard -t image/png -i "$file"
notify-send -i "$file" "Screenshot" "Copied to clipboard"
