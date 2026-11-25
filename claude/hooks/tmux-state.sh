#!/usr/bin/env bash
# Record the Claude session's state on its tmux window: working, waiting (for you) or ask
# (needs approval). tmux.conf turns @claude into an icon and color in the tab.
cat >/dev/null
[ -z "${TMUX_PANE:-}" ] && exit 0
case "$1" in
  working|waiting|ask) tmux set -w -t "$TMUX_PANE" @claude "$1" 2>/dev/null ;;
  clear)
    tmux set -wu -t "$TMUX_PANE" @claude 2>/dev/null
    tmux set -wu -t "$TMUX_PANE" @claude_title 2>/dev/null ;;
esac
exit 0
