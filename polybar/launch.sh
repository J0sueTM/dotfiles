#!/usr/bin/env bash
# One bar per connected monitor; the tray goes on the primary one.

polybar-msg cmd quit >/dev/null 2>&1
killall -q polybar
while pgrep -u "$UID" -x polybar >/dev/null; do sleep 0.1; done

primary=$(polybar --list-monitors | awk -F: '/primary/ { print $1; exit }')

for m in $(polybar --list-monitors | cut -d: -f1); do
  if [[ $m == "$primary" ]]; then
    MONITOR=$m polybar --reload main >/dev/null 2>&1 &
  else
    MONITOR=$m polybar --reload secondary >/dev/null 2>&1 &
  fi
done
