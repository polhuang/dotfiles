#!/usr/bin/bash

hyprctl dispatch exec discord
sleep 5
hyprctl dispatch workspace special:emacs & emacsclient -c -a=""
