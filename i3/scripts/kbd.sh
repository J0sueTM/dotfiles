#!/usr/bin/env bash
# usage: kbd.sh                       print the current layout for the bar (us·dvp)
#        kbd.sh next                  switch to the next layout in the cycle
#        kbd.sh set <layout> [variant]
# The bar module is custom/ipc: it only re-reads the layout when told to, so
# every switch goes through here instead of calling setxkbmap directly.

layouts=('us dvp' 'br dvorak' 'br')

current() {
  setxkbmap -query | awk '/^layout/ { l = $2 } /^variant/ { v = $2 } END { print l (v ? " " v : "") }'
}

set_layout() {
  setxkbmap "$@"
  polybar-msg -q action '#xkeyboard.hook.0' >/dev/null 2>&1
}

case ${1:-show} in
  show) current | sed 's/ /·/' ;;
  set) shift; set_layout "$@" ;;
  next)
    now=$(current) target=${layouts[0]}
    for i in "${!layouts[@]}"; do
      [[ ${layouts[$i]} == "$now" ]] && target=${layouts[$(((i + 1) % ${#layouts[@]}))]}
    done
    # shellcheck disable=SC2086 # "layout variant" splits into two args on purpose
    set_layout $target
    ;;
esac
