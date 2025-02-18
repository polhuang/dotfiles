#!/usr/bin/env sh

pids=$(hyprctl clients | grep -A5 'class: .*kitty.*' | grep 'pid' | awk '{print $2}')

# Move each "kitty" client to the "shell" workspace
for pid in $pids; do
  hyprctl dispatch movetoworkspace special:shell, pid:$pid
done
