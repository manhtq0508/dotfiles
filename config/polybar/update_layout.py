#!/bin/python

from pynput import keyboard as keybrd
from pathlib import Path as path

file = str(path("~/.config/polybar/layout.tmp").expanduser())
layout = ["Tall", "Mirror Tall", "Full", "ThreeCol"]
index = -1

# Vì 1 lý do nào đó mà pynput tự bắt phím Alt trong tổ hợp Shift + Alt thành code <65511> 
# thay vì 65513 của phím Alt nên phải xử lý thêm
accept_keys = [ keybrd.Key.cmd,
                keybrd.Key.shift,
                keybrd.Key.ctrl,
                keybrd.Key.alt,
                keybrd.Key.space,
                "<65511>" ]
current_keys = set()

def write_layout():
    global layout
    global index

    index = (index + 1) % len(layout)
    with open(file, "w", encoding="UTF-8") as f:
        f.write(layout[index])

def reset_layout():
    global layout
    global index
    
    index = -1
    write_layout()

def on_key_press(key):
    global current_keys
    
    keyStr = str(key)
    if (key in accept_keys) or (keyStr in accept_keys):
        if (keyStr == "<65511>"):
            current_keys.add(keybrd.Key.alt)
        else:
            current_keys.add(key)

    print(current_keys, len(current_keys))

    if (keybrd.Key.cmd in current_keys) and (keybrd.Key.space in current_keys):
        if len(current_keys) == 2:
            write_layout()
        elif (keybrd.Key.shift in current_keys) and (keybrd.Key.ctrl not in current_keys) and (keybrd.Key.alt not in current_keys):
            reset_layout()

def on_key_release(key):
    global current_keys

    if (str(key) != "<65511>") and (key not in current_keys):
        return 

    if str(key) == "<65511>":
        current_keys.remove(keybrd.Key.alt)
    else:
        current_keys.remove(key)

    print(current_keys, len(current_keys))


def main():
    reset_layout() # Init layout

    with keybrd.Listener(on_press=on_key_press, on_release=on_key_release) as listener:
        listener.join()

if __name__ == "__main__":
    main()
