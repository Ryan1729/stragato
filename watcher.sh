#!/bin/bash

# reload:
# https://www.npmjs.com/package/reload

# I renamed it to reload0 to deal with this issue:
# https://github.com/jprichardson/reload/issues/13
# which is about a command run at boot time in Ubuntu having the same name.
# I never actually tried to see if it affected 16.04 but didn't seem like it
# was worth the risk.

./build/parallel_commands "reload0" "./build/run_filewatcher.sh"
