#!/bin/bash

# filewatcher:
# https://github.com/thomasfl/filewatcher

filewatcher -s --interval=0.0625 '*.elm' 'elm-make Editor.elm --output=editor.js && elm-make Game.elm --output=game.js'
