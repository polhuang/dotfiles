#!/usr/bin/sh

hyprctl dispatch exec discord
sleep 5
hyprctl dispatch workspace special:emacs & emacsclient -c -a=""
 
