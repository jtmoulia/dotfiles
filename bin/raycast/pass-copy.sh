#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Pass Copy
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🔑
# @raycast.argument1 { "type": "text", "placeholder": "Password Title" }
# @raycast.packageName pass

# Documentation:
# @raycast.description Copy pass
# @raycast.author Thomas Moulia

if which -v pbcopy > /dev/null 2>&1; then
  echo "⚠️  The pbcopy utility is not installed"
  exit 1
fi

pass search "$1" | head -n1 | pbcopy
