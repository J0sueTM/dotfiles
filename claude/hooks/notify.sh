#!/usr/bin/env bash
# Claude Code hook: tell me when a session needs attention.
# Wired to the Notification (permission prompts, questions) and Stop (turn finished) events.
# Does three things:
#   1. rings the bell in the tmux pane  -> tmux flags the window, alacritty marks itself urgent for i3
#   2. desktop notification via dunst    -> clicking it jumps to the tmux window
#   3. plays a sound
# Skips 2 and 3 when you are already looking at that pane.

input=$(cat)
event=$(jq -r '.hook_event_name // empty' <<<"$input")
message=$(jq -r '.message // .last_assistant_message // empty' <<<"$input" | tr '\n' ' ' | cut -c1-200)
project=$(basename "$(jq -r '.cwd // empty' <<<"$input")")

case "$event" in
  Notification) sound=/usr/share/sounds/freedesktop/stereo/window-question.oga; urgency=normal;   title="Claude needs you" ;;
  *)            sound=/usr/share/sounds/freedesktop/stereo/complete.oga;        urgency=normal;   title="Claude is done" ;;
esac
[ -z "$message" ] && message="Waiting for input"

where=""
if [ -n "${TMUX_PANE:-}" ]; then
  where=$(tmux display -p -t "$TMUX_PANE" '#{session_name}:#{window_index} #{window_name}')
  pane_tty=$(tmux display -p -t "$TMUX_PANE" '#{pane_tty}')

  # 1. bell: tmux marks the window with "!" in the status bar
  printf '\a' >"$pane_tty" 2>/dev/null

  # Is this pane on screen in a focused terminal? Then don't nag.
  visible=$(tmux display -p -t "$TMUX_PANE" '#{&&:#{pane_active},#{window_active}}')
  if [ "$visible" = 1 ] && command -v xdotool >/dev/null; then
    focused_pid=$(xdotool getactivewindow getwindowpid 2>/dev/null)
    session=$(tmux display -p -t "$TMUX_PANE" '#{session_name}')
    for client_pid in $(tmux list-clients -t "$session" -F '#{client_pid}' 2>/dev/null); do
      pid=$client_pid
      while [ -n "$pid" ] && [ "$pid" -gt 1 ]; do
        [ "$pid" = "$focused_pid" ] && exit 0
        pid=$(ps -o ppid= -p "$pid" | tr -d ' ')
      done
    done
  fi
fi

# 3. sound
(paplay "$sound" >/dev/null 2>&1 &)

# 2. notification; click it to jump to the window (runs detached so the hook returns immediately)
(
  action=$(notify-send -a "Claude Code" -u "$urgency" -t 15000 -i dialog-information \
    -A default=Jump "$title${project:+ · $project}" "${where:+[$where] }$message" 2>/dev/null)
  if [ "$action" = default ] && [ -n "${TMUX_PANE:-}" ]; then
    i3-msg '[urgent=latest] focus' >/dev/null 2>&1
    tmux switch-client -t "$TMUX_PANE" 2>/dev/null
    tmux select-window -t "$TMUX_PANE" \; select-pane -t "$TMUX_PANE"
  fi
) >/dev/null 2>&1 </dev/null &
disown

exit 0
