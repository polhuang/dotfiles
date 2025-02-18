#!/usr/bin/env sh

pids=$(hyprctl clients | grep -A5 'class: .*kitty.*' | grep -B9 -v 'workspace: -83 (special:admin)' | grep 'pid' | awk '{print $2}')

# Move each "kitty" client to the "shell" workspace
for pid in $pids; do
  hyprctl dispatch movetoworkspace special:aquarium, pid:$pid
done
