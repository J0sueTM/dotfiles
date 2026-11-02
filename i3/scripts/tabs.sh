#!/usr/bin/env bash
# Polybar tab list (custom/script, tail = true).
#
# i3's own tab row is hidden (picom makes it transparent), so when the focused
# window lives in a tabbed or stacked container its siblings are listed here:
# the active one underlined in gold, the rest muted. Click a title to focus it.
# Prints an empty line otherwise, so the module disappears.

gold=#e6b450 fg=#dfe3dc muted=#6f7d78 border=#2a3331

# prints "<con id>|<1 if active>|<title>" per tab of the focused container
query() {
  i3-msg -t get_tree | jq -r '
    def kids: .nodes[]?, .floating_nodes[]?;
    [recurse(kids)
      | select((.layout == "tabbed" or .layout == "stacked")
               and any(recurse(kids); .focused))]
    | last // empty
    | .focus[0] as $active
    | .nodes[]
    | [(.id | tostring),
       (if .id == $active then "1" else "0" end),
       ((.name // first(recurse(kids) | select(.window != null) | .name) // "…")
        | sub(" [-—] [^-—]+$"; "")      # drop the " - Brave" app suffix
        | gsub("[|{}]"; ""))]
    | join("|")'
}

render() {
  local -a ids active titles
  local id a t
  while IFS='|' read -r id a t; do
    ids+=("$id") active+=("$a") titles+=("$t")
  done < <(query)

  local n=${#ids[@]}
  ((n == 0)) && { echo; return; }

  # ~90 characters shared between the tabs, so the clock never gets pushed
  local width=$((90 / n)) out="%{F$border}│%{F-}"
  ((width > 40)) && width=40
  for ((i = 0; i < n; i++)); do
    t=${titles[$i]}
    ((${#t} > width)) && t="${t:0:width-1}…"
    out+=" %{A1:i3-msg -q [con_id=${ids[$i]}] focus:}"
    if [[ ${active[$i]} == 1 ]]; then
      out+="%{u$gold}%{+u}%{F$fg} $t %{F-}%{-u}"
    else
      out+="%{F$muted} $t %{F-}"
    fi
    out+="%{A}"
  done
  echo "$out"
}

# loop so the module survives `i3-msg restart` (the subscription dies with it)
while true; do
  render
  i3-msg -t subscribe -m '["window", "workspace", "binding"]' | while read -r _; do
    render
  done
  sleep 1
done
