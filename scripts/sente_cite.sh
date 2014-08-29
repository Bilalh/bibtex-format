#!/bin/bash
osascript <<APPLESCRIPT | 
tell application "Sente 6"
    set refs to select references current library as tags
	create bibliography elements current library from tags refs with "LaTeX" for bibliography
end tell
APPLESCRIPT
sed 's/<br>/\'$'\n/g' && osascript <<APPLESCRIPT 
tell application "System Events" to keystroke tab using command down
APPLESCRIPT
