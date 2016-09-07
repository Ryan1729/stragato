#!/bin/bash

# filewatcher:
# https://github.com/thomasfl/filewatcher

filewatcher -s --interval=0.0625 '*.elm' 'elm-make Main.elm --output=elm.js'
