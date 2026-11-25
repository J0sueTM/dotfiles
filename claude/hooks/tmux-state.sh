#!/usr/bin/env bash
# Show the Claude session state in its tmux tab: ⚙ working, ● waiting for you, ? needs approval.
cat >/dev/null
[ -z "${TMUX_PANE:-}" ] && exit 0
case "$1" in
  working) mark='⚙ ' ;;
  waiting) mark='● ' ;;
  ask)     mark='? ' ;;
  clear)   tmux set -wu -t "$TMUX_PANE" @claude 2>/dev/null; tmux set -wu -t "$TMUX_PANE" @claude_title 2>/dev/null; exit 0 ;;
esac
tmux set -w -t "$TMUX_PANE" @claude "$mark" 2>/dev/null
exit 0
