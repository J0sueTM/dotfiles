#!/usr/bin/env bash
# Polybar workspace indicator (custom/script, tail = true).
#
# All five greek workspaces are always shown; each carries a superscript count
# of its windows (γ³). Focused is bold gold with a notch hanging from the top
# edge, visible-on-another-monitor is teal, occupied is bright, empty fades
# into the bar, urgent is red. Click to switch, scroll to cycle.
# Only workspaces on this bar's $MONITOR are drawn (empty ones on every bar).

names=('1:α' '2:β' '3:γ' '4:δ' '5:ε')
sup=(' ' '¹' '²' '³' '⁴' '⁵' '⁶' '⁷' '⁸' '⁹')

gold=#e6b450 gold_dim=#a8843c teal=#5fb3a1 red=#e5675f
fg=#dfe3dc muted=#6f7d78 empty=#2f3936

mode=''

# prints "<name>|<state>|<output>|<window count>" per workspace
query() {
  jq -nr \
    --argjson ws "$(i3-msg -t get_workspaces)" \
    --argjson tree "$(i3-msg -t get_tree)" \
    --argjson names "$(printf '%s\n' "${names[@]}" | jq -R . | jq -s .)" '
    ($ws | map({(.name): .}) | add // {}) as $w
    | ([$tree | recurse(.nodes[]?) | select(.type == "workspace")
        | {(.name): ([recurse(.nodes[]?, .floating_nodes[]?) | select(.window != null)] | length)}]
       | add // {}) as $count
    | ($names + (($ws | map(.name)) - $names))[] as $n
    | ($w[$n] // null) as $x
    | [$n,
       (if $x == null then "empty"
        elif $x.urgent then "urgent"
        elif $x.focused then "focused"
        elif $x.visible then "visible"
        elif ($count[$n] // 0) == 0 then "empty"
        else "idle" end),
       ($x.output // ""),
       ($count[$n] // 0 | tostring)]
    | join("|")'
}

render() {
  local out='' name state output count letter n color
  while IFS='|' read -r name state output count; do
    [[ -n $MONITOR && -n $output && $output != "$MONITOR" ]] && continue
    letter=${name#*:}
    n=${sup[$count]:-⁺}
    ((count > 9)) && n=⁺
    action="%{A1:i3-msg -q workspace number ${name%%:*}:}"
    case $state in
      focused) out+="$action%{o$gold}%{+o}%{F$gold}%{T2} $letter%{T-}%{F$gold_dim}$n%{F-}%{-o}%{A}" ;;
      visible) out+="$action%{F$teal} $letter%{F$muted}$n%{F-}%{A}" ;;
      urgent) out+="$action%{F$red} $letter$n%{F-}%{A}" ;;
      idle) out+="$action%{F$fg} $letter%{F$muted}$n%{F-}%{A}" ;;
      *) out+="$action%{F$empty} $letter$n%{F-}%{A}" ;;
    esac
  done < <(query)

  [[ -n $mode ]] && out+="  %{B$gold}%{F#0b0e0d} $mode %{F-}%{B-}"
  echo "%{A4:i3-msg -q workspace prev_on_output:}%{A5:i3-msg -q workspace next_on_output:}$out %{A}%{A}"
}

# loop so the bar survives `i3-msg restart` (the subscription dies with it)
while true; do
  render
  i3-msg -t subscribe -m '["workspace", "window", "mode", "output"]' | while read -r event; do
    case $event in
      *'"pango_markup"'*) # mode event
        mode=$(jq -r '.change' <<<"$event")
        [[ $mode == default ]] && mode=''
        ;;
      # window focus/title/mark changes don't affect the indicator (workspace
      # focus events start with {"change":"focus","current":... and still render)
      '{"change":"title","container"'* | '{"change":"focus","container"'* | '{"change":"mark","container"'*) continue ;;
    esac
    render
  done
  sleep 1
done
