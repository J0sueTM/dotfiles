#!/usr/bin/env bash
# usage: kbd.sh        print the current layout for the bar (us·dvp, br·dvorak, br)
#        kbd.sh next   switch to the next layout in the cycle

layouts=('us dvp' 'br dvorak' 'br')

current() {
  setxkbmap -query | awk '/^layout/ { l = $2 } /^variant/ { v = $2 } END { print l (v ? " " v : "") }'
}

case ${1:-show} in
  show) current | sed 's/ /·/' ;;
  next)
    now=$(current)
    for i in "${!layouts[@]}"; do
      if [[ ${layouts[$i]} == "$now" ]]; then
        # shellcheck disable=SC2086 # "layout variant" splits into two args on purpose
        setxkbmap ${layouts[$(((i + 1) % ${#layouts[@]}))]}
        exit
      fi
    done
    setxkbmap ${layouts[0]}
    ;;
esac
