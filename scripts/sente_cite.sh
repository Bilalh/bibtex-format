#!/bin/bash
printf '\cite{'
osascript <<APPLESCRIPT | 
tell application "Sente 6"
    set refs to select references current library as tags
	create bibliography elements current library from tags refs with "LaTeXKeys" for bibliography
end tell
APPLESCRIPT
sed 's/<br>/\, /g' | ghead -c -1 && printf '}' && osascript <<APPLESCRIPT 
tell application "System Events" to keystroke tab using command down
APPLESCRIPT
