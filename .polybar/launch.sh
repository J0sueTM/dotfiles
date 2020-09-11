#!/bin/bash

# kill all running bar instances
killall -q polybar

# wait untill all are closed
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch
polybar makebar &

echo "Polybar launched ..."
