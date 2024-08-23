#!/bin/python

from pynput import keyboard

layoutIndex = 0
LAYOUT = ["Tall", "Mirror Tall", "Full", "ThreeCol"]
LAYOUT_FILE = "/home/manhtq/.config/polybar/layout.tmp"

def resetLayout():
    global LAYOUT

    with open(LAYOUT_FILE, "w", encoding="UTF-8") as f:
        f.write(LAYOUT[0])

def update():
    print("Catch")
    global LAYOUT
    global layoutIndex

    layoutIndex += 1
    if layoutIndex >= len(LAYOUT):
        layoutIndex = 0

    with open(LAYOUT_FILE, "w", encoding="UTF-8") as f:
        f.write(LAYOUT[layoutIndex])

resetLayout()

with keyboard.GlobalHotKeys({
    '<cmd>+<shift>+<space>': lambda: print("Gill"),
    '<cmd>+<space>': update
    }) as l:
    l.join()
    print(l)
