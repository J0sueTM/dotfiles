#!/usr/bin/env bash
# Pointer behaviour matching GNOME: natural (reversed) scrolling on touchpad
# and mouse, tap-to-click on the touchpad.

xinput list --id-only | while read -r id; do
  props=$(xinput list-props "$id" 2>/dev/null)
  if [[ $props == *'libinput Natural Scrolling Enabled ('* ]]; then
    xinput set-prop "$id" 'libinput Natural Scrolling Enabled' 1
  fi
  if [[ $props == *'libinput Tapping Enabled ('* ]]; then
    xinput set-prop "$id" 'libinput Tapping Enabled' 1
  fi
done
