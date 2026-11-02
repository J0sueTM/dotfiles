#!/usr/bin/env bash
# Wi-Fi picker (click the wifi icon in the bar). Uses NetworkManager via nmcli.

notify() { notify-send -a wifi "Wi-Fi" "$1"; }

signal_icon() {
  if (($1 >= 80)); then echo 󰤨
  elif (($1 >= 60)); then echo 󰤥
  elif (($1 >= 40)); then echo 󰤢
  elif (($1 >= 20)); then echo 󰤟
  else echo 󰤯
  fi
}

if [[ $(nmcli radio wifi) == enabled ]]; then
  entries=('󰖪  Turn Wi-Fi off')
  actions=(off)
else
  entries=('󰖩  Turn Wi-Fi on')
  actions=(on)
fi
entries+=('󰒓  Network settings')
actions+=(settings)

declare -A seen

# IN-USE:SIGNAL:SECURITY:SSID, ssid last so colons inside it are kept
while IFS= read -r line; do
  [[ $line =~ ^([^:]*):([^:]*):([^:]*):(.+)$ ]] || continue
  in_use=${BASH_REMATCH[1]} signal=${BASH_REMATCH[2]} security=${BASH_REMATCH[3]}
  ssid=${BASH_REMATCH[4]//\\:/:}
  [[ -n ${seen[$ssid]} ]] && continue # same network, another access point
  seen[$ssid]=1
  mark='  '
  [[ $in_use == '*' ]] && mark='󰄬 '
  lock=''
  [[ -n $security && $security != -- ]] && lock='  󰌾'
  entries+=("$mark$(signal_icon "$signal")  $ssid$lock")
  actions+=("ssid:$ssid")
done < <(nmcli --terse --fields IN-USE,SIGNAL,SECURITY,SSID device wifi list --rescan auto 2>/dev/null | LC_ALL=C sort -t: -k1,1r -k2,2nr)

index=$(printf '%s\n' "${entries[@]}" | rofi -dmenu -i -p 'wi-fi' -format i -no-custom \
  -theme-str 'window { width: 420px; } listview { lines: 10; }') || exit 0
action=${actions[$index]}

case $action in
  on) nmcli radio wifi on ;;
  off) nmcli radio wifi off ;;
  settings) nm-connection-editor & ;;
  ssid:*)
    ssid=${action#ssid:}
    if nmcli --terse --fields NAME connection show | grep -qxF "$ssid"; then
      nmcli connection up id "$ssid" >/dev/null 2>&1 && notify "Connected to $ssid" || notify "Could not connect to $ssid"
    elif [[ ${entries[$index]} == *󰌾 ]]; then
      password=$(rofi -dmenu -password -p "$ssid" -theme-str 'window { width: 420px; } listview { enabled: false; }') || exit 0
      nmcli device wifi connect "$ssid" password "$password" >/dev/null 2>&1 &&
        notify "Connected to $ssid" || notify "Could not connect to $ssid"
    else
      nmcli device wifi connect "$ssid" >/dev/null 2>&1 && notify "Connected to $ssid" || notify "Could not connect to $ssid"
    fi
    ;;
esac
