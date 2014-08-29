#!/bin/bash

osascript <<APPLESCRIPT | 
tell application "Sente 6"
    set refs to select references current library as bibtex
end tell
APPLESCRIPT
osascript <<APPLESCRIPT 
tell application "System Events" to keystroke tab using command down
APPLESCRIPT

