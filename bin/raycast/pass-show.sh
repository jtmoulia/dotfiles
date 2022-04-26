#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Pass Show
# @raycast.mode fullOutput

# Optional parameters:
# @raycast.icon 🔑
# @raycast.argument1 { "type": "text", "placeholder": "Password Title" }
# @raycast.packageName pass

# Documentation:
# @raycast.description Show pass
# @raycast.author Thomas Moulia

pass show "$1"
