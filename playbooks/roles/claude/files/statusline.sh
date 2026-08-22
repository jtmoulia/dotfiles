#!/bin/bash
# Claude Code status line

input=$(cat)

# ANSI colors
reset='\033[0m'
dim='\033[2m'
cyan='\033[36m'
green='\033[32m'
yellow='\033[33m'
magenta='\033[35m'
red='\033[31m'
blue='\033[34m'

# Extract values via jq
duration_ms=$(echo "$input" | jq -r '.cost.total_duration_ms // 0')
cost=$(echo "$input" | jq -r '.cost.total_cost_usd // 0')
context_pct=$(echo "$input" | jq -r '.context_window.used_percentage // 0')
cwd=$(echo "$input" | jq -r '.cwd // empty')
model=$(echo "$input" | jq -r '.model.display_name // empty')
lines_added=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
lines_removed=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')

# Format duration: convert ms to human-readable
total_secs=$(( duration_ms / 1000 ))
hours=$(( total_secs / 3600 ))
mins=$(( (total_secs % 3600) / 60 ))
secs=$(( total_secs % 60 ))
if (( hours > 0 )); then
  duration="${hours}h${mins}m"
elif (( mins > 0 )); then
  duration="${mins}m${secs}s"
else
  duration="${secs}s"
fi

# Format cost
cost_fmt=$(printf '$%.2f' "$cost")

# Format context percentage as integer with colored bar
context_int=$(printf '%.0f' "$context_pct")
filled=$(( context_int / 10 ))
empty=$(( 10 - filled ))
# Color the bar based on usage level
if (( context_int >= 80 )); then
  bar_color="$red"
elif (( context_int >= 50 )); then
  bar_color="$yellow"
else
  bar_color="$green"
fi
bar_filled=$(printf '%0.s█' $(seq 1 $filled 2>/dev/null))
bar_empty=$(printf '%0.s░' $(seq 1 $empty 2>/dev/null))

# Show remaining percentage until compaction threshold (~95%)
compaction_threshold=95
compaction_remaining=""
remaining_pct=$(( compaction_threshold - context_int ))
if (( remaining_pct >= 0 && remaining_pct < 10 )); then
  compaction_remaining="${remaining_pct}%"
fi

# Get git branch
git_branch=""
if [ -n "$cwd" ]; then
  git_branch=$(git -C "$cwd" rev-parse --abbrev-ref HEAD 2>/dev/null)
fi

# Get active workspace
WORKSPACE_BIN="$HOME/.claude/bin/workspace"
workspace=""
if [ -n "$cwd" ] && [ -x "$WORKSPACE_BIN" ]; then
  workspace=$("$WORKSPACE_BIN" --root "$cwd" status 2>/dev/null)
  if [ "$workspace" = "none" ] || [ -z "$workspace" ]; then
    workspace=""
  fi
fi

# Build output with colors and emoji
parts=()

# cwd, git branch, and workspace first
if [ -n "$cwd" ]; then
  dir_part="📂 ${blue}$(basename "$cwd")${reset}"
  if [ -n "$git_branch" ]; then
    dir_part="${dir_part} ${dim}(${yellow}${git_branch}${reset}${dim})${reset}"
  fi
  parts+=("$dir_part")
fi
if [ -n "$workspace" ]; then
  parts+=("🗂  ${magenta}${workspace}${reset}")
fi

# model
if [ -n "$model" ]; then
  parts+=("${dim}${model}${reset}")
fi

# then metrics
parts+=("⏱  ${cyan}${duration}${reset}")
parts+=("💰 ${green}${cost_fmt}${reset}")
if (( lines_added > 0 || lines_removed > 0 )); then
  parts+=("${green}+${lines_added}${reset} ${red}-${lines_removed}${reset}")
fi
if [ -n "$compaction_remaining" ]; then
  parts+=("📊 ${bar_color}${bar_filled}${dim}${bar_empty}${reset} ${bar_color}${context_int}%${reset} ${dim}(${compaction_remaining} left)${reset}")
else
  parts+=("📊 ${bar_color}${bar_filled}${dim}${bar_empty}${reset} ${bar_color}${context_int}%${reset}")
fi

# Join with dim separator
sep=$(printf " ${dim}│${reset} ")
result=""
for i in "${!parts[@]}"; do
  if [ $i -eq 0 ]; then
    result="${parts[$i]}"
  else
    result="${result}${sep}${parts[$i]}"
  fi
done

echo -e "$result"
