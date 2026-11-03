#!/usr/bin/env bash
# Follows the charger: plugged in = performance profile and the panel at its
# fastest refresh rate; on battery = balanced profile and 60Hz.
# Polls the adapter every 5s (a file read, no processes spawned).

ac=/sys/class/power_supply/ACAD/online

# replace the instance from before an i3 restart
for pid in $(pgrep -xf 'bash \S*/i3/scripts/power\.sh'); do
  ((pid != $$)) && kill "$pid" 2>/dev/null
done

# mode id with the panel's current resolution at <rate> Hz ("max" = fastest).
# Modes are picked by id: several share the name "1920x1200", and xrandr
# --mode/--rate by name always lands on the first (fastest) one.
mode_id() {
  xrandr --verbose --current | awk -v o="$1" -v want="$2" '
    $1 == o { found = 1; next }
    found && /^[^ \t]/ { exit }
    found && /^  [0-9]+x[0-9]+ \(0x/ { name = $1; id = $2; gsub(/[()]/, "", id); cur = /\*current/ }
    found && cur { res = name }
    found && /^ +v:/ {
      hz = $NF; sub(/Hz/, "", hz)
      ids[++n] = id; names[n] = name; rates[n] = hz + 0
    }
    END {
      for (i = 1; i <= n; i++) {
        if (names[i] != res) continue
        if (want == "max" && rates[i] > best) { best = rates[i]; pick = ids[i] }
        if (want != "max" && !pick && int(rates[i] + 0.5) == want) pick = ids[i]
      }
      print pick
    }'
}

apply() {
  local panel id
  panel=$(xrandr --current | awk '/^eDP/ { print $1; exit }')
  if [[ $1 == 1 ]]; then
    powerprofilesctl set performance
    [[ -n $panel ]] && id=$(mode_id "$panel" max)
  else
    powerprofilesctl set balanced
    [[ -n $panel ]] && id=$(mode_id "$panel" 60)
  fi
  [[ -n $id ]] && xrandr --output "$panel" --mode "$id"
  return 0
}

# let monitors.sh (started alongside on i3 start/restart) lay out the panel first
sleep 3

last=''
while true; do
  read -r now <"$ac"
  [[ $now != "$last" ]] && apply "$now" && last=$now
  sleep 5
done
