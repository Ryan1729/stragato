#!/bin/bash

# filewatcher:
# https://github.com/thomasfl/filewatcher

filewatcher '*.elm' 'elm-make Main.elm --output=elm.js'
