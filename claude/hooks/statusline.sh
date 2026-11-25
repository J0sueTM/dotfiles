#!/usr/bin/env bash
# Claude Code status line, in the terminal's own ANSI palette (alacritty = i3 colors):
#   yellow = gold accent, cyan = teal, red = alert, bright black = muted, white = text.
#  Opus 5.5 ·  ~/dev/project ·  main* · ▰▰▰▱▱▱▱▱ 34% · $1.20 · 󰑕 session-name

input=$(cat)
j() { jq -r "$1 // empty" <<<"$input"; }

model=$(j '.model.display_name')
dir=$(j '.workspace.current_dir // .cwd')
pct=$(j '.context_window.used_percentage')
cost=$(j '.cost.total_cost_usd')
five=$(j '.rate_limits.five_hour.used_percentage')

E=$'\e'
reset="$E[0m"; muted="$E[90m"; text="$E[37m"; bold="$E[1m"
gold="$E[33m"; teal="$E[36m"; red="$E[31m"; green="$E[32m"
sep="${muted} · ${reset}"

out="${gold}${bold}󰚩 ${model:-Claude}${reset}"

if [ -n "$dir" ]; then
  out+="${sep}${text} ${dir/#$HOME/\~}${reset}"
  branch=$(git -C "$dir" --no-optional-locks branch --show-current 2>/dev/null)
  if [ -n "$branch" ]; then
    dirty=$(git -C "$dir" --no-optional-locks status --porcelain 2>/dev/null | head -1)
    out+="${sep}${teal} ${branch}${dirty:+${gold}*}${reset}"
  fi
fi

if [ -n "$pct" ]; then
  p=${pct%.*}
  if   [ "$p" -ge 80 ]; then c=$red
  elif [ "$p" -ge 50 ]; then c=$gold
  else c=$teal; fi
  filled=$(( (p + 6) / 12 )); [ $filled -gt 8 ] && filled=8
  bar=""; for i in 1 2 3 4 5 6 7 8; do
    if [ $i -le $filled ]; then bar+="▰"; else bar+="${muted}▱${c}"; fi
  done
  out+="${sep}${c}${bar} ${p}%${reset}"
fi

if [ -n "$cost" ]; then
  out+="${sep}${green}\$$(printf '%.2f' "$cost")${reset}"
fi

if [ -n "$five" ] && [ "${five%.*}" -ge 50 ]; then
  out+="${sep}${red}󰔟 5h ${five%.*}%${reset}"
fi

if [ -n "${TMUX_PANE:-}" ]; then
  name=$(tmux show -wv -t "$TMUX_PANE" @claude_title 2>/dev/null)
  [ -n "$name" ] && out+="${sep}${muted}󰑕 ${name}${reset}"
fi

printf '%s' "$out"
