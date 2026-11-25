#!/usr/bin/env bash
# Stop hook: give the session a resumable name and show it in the tmux tab.
#   - If you named it with /rename, that name wins.
#   - Otherwise, after MIN_PROMPTS prompts, turn Claude's auto title ("ai-title")
#     into a slug and save it as the session's name, exactly where /rename saves it.
MIN_PROMPTS=2
MAX_SLUG=40

input=$(cat)
transcript=$(jq -r '.transcript_path // empty' <<<"$input")
session_id=$(jq -r '.session_id // empty' <<<"$input")
[ -f "$transcript" ] || exit 0

IFS="|" read -r custom ai prompts < <(jq -rs '
  [ .[] | select(.type=="custom-title") | .customTitle ] as $c
  | [ .[] | select(.type=="ai-title") | .aiTitle ] as $a
  | [ .[] | select(.type=="user" and (.isMeta|not) and (.message.content
        | if type=="string" then true else any(.[]; .type=="text") end)) ] as $u
  | [ ($c|last // "" | @base64), ($a|last // "" | @base64), ($u|length|tostring) ] | join("|")
' "$transcript")
custom=$(base64 -d <<<"$custom"); ai=$(base64 -d <<<"$ai")

name=$custom
if [ -z "$name" ] && [ -n "$ai" ] && [ "${prompts:-0}" -ge "$MIN_PROMPTS" ]; then
  slug=$(tr '[:upper:]' '[:lower:]' <<<"$ai" | sed -E 's/[^a-z0-9]+/-/g; s/^-+|-+$//g')
  if [ ${#slug} -gt $MAX_SLUG ]; then
    slug=${slug:0:$MAX_SLUG}; slug=${slug%-*}   # cut at a word boundary
  fi
  if [ -n "$slug" ]; then
    jq -cn --arg t "$slug" --arg s "$session_id" '{type:"custom-title",customTitle:$t,sessionId:$s}' >>"$transcript"
    name=$slug
  fi
fi

[ -n "${TMUX_PANE:-}" ] && [ -n "$name" ] && tmux set -w -t "$TMUX_PANE" @claude_title "$name" 2>/dev/null
exit 0
