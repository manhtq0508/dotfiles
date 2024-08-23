#!/bin/python

from pynput import keyboard as keybrd
from pathlib import Path as path

current_press = []

file = str(path("~/.config/polybar/layout.tmp").expanduser())
layout = ["Tall", "Mirror Tall", "Full", "ThreeCol"]
index = -1

def write_layout():
    global layout
    global index

    index += 1
    if index >= len(layout):
        index = 0

    with open(file, "w", encoding="UTF-8") as f:
        f.write(layout[index])

def on_key_press(key):
    if key not in current_press:
        current_press.append(key)

    if (keybrd.Key.cmd in current_press) and (keybrd.Key.space in current_press) and (len(current_press) == 2):
        write_layout()

def on_key_release(key):
    if key in current_press:
        current_press.remove(key)

write_layout() # Init layout

with keybrd.Listener(
    on_press=on_key_press,
    on_release=on_key_release
    ) as listener:
    listener.join()
