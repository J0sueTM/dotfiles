#!/usr/bin/env bash
# Session menu (mod+Escape, or the power icon in the bar).

lock='󰌾  Lock'
suspend='󰤄  Suspend'
logout='󰍃  Log out'
reboot='󰜉  Reboot'
poweroff='󰐥  Shut down'

choice=$(printf '%s\n' "$lock" "$suspend" "$logout" "$reboot" "$poweroff" |
  rofi -dmenu -i -p 'session' -no-custom -theme-str '
    window   { width: 280px; }
    listview { lines: 5; }
    inputbar { enabled: false; }')

case $choice in
  "$lock") loginctl lock-session ;;
  "$suspend") systemctl suspend ;;
  "$logout") i3-msg exit ;;
  "$reboot") systemctl reboot ;;
  "$poweroff") systemctl poweroff ;;
esac
