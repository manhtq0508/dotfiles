#!/bin/python

from pathlib import Path
import os

try:
    from pynput import keyboard
except ImportError:
    os.system("echo pynput not isntalled >> /tmp/layout.tmp")
    os.exit(0)

FILE = "/tmp/layout.tmp"

# pynput bắt phím alt trong tổ hợp Shift + Alt là <65511> nên phải xử lý thêm 1 trường hợp
SPECIAL_ALT_KEYCODE = "<65511>"
ACCEPT_MOD_KEYS = { keyboard.Key.cmd,
                    keyboard.Key.ctrl,
                    keyboard.Key.alt,
                    keyboard.Key.shift,
                    keyboard.Key.space }
ACCEPT_WORKSPACE_KEYS =  { keyboard.KeyCode.from_char(str(i)) for i in range(1, 10) }

LAYOUT = [ "Tall", "Mirror Tall", "Grid", "Full", "ThreeCol", "Mouse Tall", "Mirror MTall"]
RESET_LAYOUT_VALUE = -1

workspace = 0
workspace_layout = [ RESET_LAYOUT_VALUE for i in range(9) ]
current_keys = set()

def change_layout():
    workspace_layout[workspace] = (workspace_layout[workspace] + 1) % len(LAYOUT)
    
    with open(FILE, "w", encoding="UTF-8") as f:
        f.write(LAYOUT[workspace_layout[workspace]])

def change_workspace(ws_index: int):
    global workspace
    global workspace_layout

    workspace = ws_index
    workspace_layout[workspace] += RESET_LAYOUT_VALUE if workspace_layout[workspace] != -1 else 0
    change_layout()

def reset_layout():
    global workspace_layout
    
    workspace_layout[workspace] = RESET_LAYOUT_VALUE
    change_layout()

def key_press(key):
    keyStr = str(key)
    if keyStr == SPECIAL_ALT_KEYCODE:
        current_keys.add(keyboard.Key.alt)
    elif (key in ACCEPT_MOD_KEYS) or (key in ACCEPT_WORKSPACE_KEYS):
        current_keys.add(key)

    if (keyboard.Key.cmd in current_keys) and (keyboard.Key.space in current_keys):
        if len(current_keys) == 2: # cmd + space
            change_layout() 
        elif (keyboard.Key.shift in current_keys) and (keyboard.Key.ctrl not in current_keys) and (keyboard.Key.alt not in current_keys): # cmd + shift + space
            reset_layout()
    elif (keyboard.Key.cmd in current_keys) and (keyboard.Key.alt not in current_keys) and (keyboard.Key.space not in current_keys) and (keyboard.Key.ctrl not in current_keys) and (keyboard.Key.space not in current_keys):
        for i in ACCEPT_WORKSPACE_KEYS:
            if i in current_keys:
                change_workspace(int(str(i).replace("'", "")) - 1)

def key_release(key):
    if str(key) == SPECIAL_ALT_KEYCODE:
        current_keys.remove(keyboard.Key.alt)
    elif key in current_keys:
        current_keys.remove(key)

def main():
    reset_layout()

    with keyboard.Listener(on_press=key_press, on_release=key_release) as listener:
        listener.join()

if __name__ == "__main__":
    main()
