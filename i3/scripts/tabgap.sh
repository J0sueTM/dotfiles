#!/usr/bin/env bash
# Keeps the top gap the same for tabbed/stacked layouts as for plain windows.
#
# i3 still reserves room for the tab row of tabbed/stacked containers even
# though picom hides it (8px, or 8px per window when stacked). When that row
# sits at the top of the focused workspace, pull the workspace up by the same
# amount with a negative top gap. Mixed rows (a tabbed column beside a plain
# window) are left alone, since moving them would misalign the plain one.

# replace the instance from before an i3 restart
for pid in $(pgrep -xf 'bash \S*/i3/scripts/tabgap\.sh'); do
  ((pid != $$)) && kill "$pid" 2>/dev/null
done

# space taken above the topmost window of a container
offset() {
  i3-msg -t get_tree | jq -r '
    def top:
      if (.nodes | length) == 0 then 0
      elif .layout == "splith" then [.nodes[] | top] | min
      elif .layout == "splitv" then .nodes[0] | top
      else
        (if .layout == "stacked" then [.nodes[].deco_rect.height] | add
         else .nodes[0].deco_rect.height end)
        + (.focus[0] as $f | first((.nodes[] | select(.id == $f) | top), 0))
      end;
    first(.. | objects
          | select(.type == "workspace" and any(.. | objects; .focused == true)))
    | "\(.gaps.top) \(top)"'
}

apply() {
  local current needed
  read -r current needed < <(offset) || return
  ((current != -needed)) && i3-msg -q "gaps top current set $((-needed))"
}

# loop so it survives `i3-msg restart`
while true; do
  apply
  i3-msg -t subscribe -m '["window", "workspace", "binding"]' | while read -r event; do
    case $event in '{"change":"title"'* | '{"change":"mark"'*) continue ;; esac
    apply
  done
  sleep 1
done
