#!/bin/bash

osascript <<APPLESCRIPT | 
tell application "Sente 6"
    set refs to retrieve selected references current library as bibtex
end tell
APPLESCRIPT
bibtex-format | align_equals.rb