#!/usr/bin/env bash
# Claude Code status line: rounded pills in the terminal's own ANSI palette
# (alacritty = i3 colors): yellow = gold accent, cyan = teal, red = alert,
# black = raised surface, bright black = muted.
#   ( Opus 5.5 ) ( ~/dev/project ) ( main* ) ( ━━━───── 34% ) session-name

input=$(cat)
j() { jq -r "$1 // empty" <<<"$input"; }

model=$(j '.model.display_name')
dir=$(j '.workspace.current_dir // .cwd')
pct=$(j '.context_window.used_percentage')
five=$(j '.rate_limits.five_hour.used_percentage')

E=$'\e'
reset="$E[0m"
L=$''; R=$''   # rounded pill caps (Nerd Font)

# pill <bg 0-7> <content, already colored for that background>
pill() { printf '%s' "$E[3${1}m${L}$E[4${1}m${2}$E[0;3${1}m${R}${reset}"; }

out=$(pill 3 "$E[30;1m"$'\U000f06a9'"  ${model:-Claude}")

if [ -n "$dir" ]; then
  short=${dir/#$HOME/\~}
  IFS=/ read -ra parts <<<"$short"
  n=${#parts[@]}
  [ "$n" -gt 4 ] && short="…/${parts[n-2]}/${parts[n-1]}"
  out+=" $(pill 0 "$E[37m"$''"  ${short}")"
  branch=$(git -C "$dir" --no-optional-locks branch --show-current 2>/dev/null)
  if [ -n "$branch" ]; then
    dirty=$(git -C "$dir" --no-optional-locks status --porcelain 2>/dev/null | head -1)
    out+=" $(pill 0 "$E[36m"$''" ${branch}${dirty:+$E[33m*}")"
  fi
fi

if [ -n "$pct" ]; then
  p=${pct%.*}
  if   [ "$p" -ge 80 ]; then c=31
  elif [ "$p" -ge 50 ]; then c=33
  else c=36; fi
  filled=$(( (p + 6) / 12 )); [ $filled -gt 8 ] && filled=8
  bar="$E[${c}m"; for i in 1 2 3 4 5 6 7 8; do
    [ $i -eq $((filled + 1)) ] && bar+="$E[90m"
    if [ $i -le $filled ]; then bar+="━"; else bar+="─"; fi
  done
  out+=" $(pill 0 "${bar} $E[${c}m${p}%")"
fi

if [ -n "$five" ] && [ "${five%.*}" -ge 50 ]; then
  out+=" $(pill 1 "$E[30;1m"$'\U000f051f'"  5h ${five%.*}%")"
fi

if [ -n "${TMUX_PANE:-}" ]; then
  name=$(tmux show -wv -t "$TMUX_PANE" @claude_title 2>/dev/null)
  [ ${#name} -gt 30 ] && name="${name:0:29}…"
  [ -n "$name" ] && out+="  $E[90m${name}${reset}"
fi

printf '%s' "$out"
