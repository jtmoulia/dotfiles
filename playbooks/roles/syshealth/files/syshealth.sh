#!/usr/bin/env bash
# syshealth.sh - Arch Linux system health monitor
# Generates concise status summaries for LLM consumption
# Only reports anomalies; silent when everything is healthy

set -euo pipefail

# Configuration
DISK_WARN_THRESHOLD=80  # Warn when disk usage exceeds this percentage
PACMAN_WARN_THRESHOLD=20  # Warn when more than this many packages are outdated
PACMAN_CRITICAL_THRESHOLD=50  # Critical when more than this many packages are outdated

# Output helpers
output_json=false
output_brief=false

# Parse args
while [[ $# -gt 0 ]]; do
    case $1 in
        --json) output_json=true; shift ;;
        --brief) output_brief=true; shift ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

# Anomaly tracking
declare -a anomalies=()
declare -a warnings=()
declare -a info=()

# Check 1: Package updates
check_packages() {
    local outdated_count=0
    local security_updates=0
    
    if command -v checkupdates &> /dev/null; then
        outdated_count=$(checkupdates 2>/dev/null | wc -l || echo 0)
    fi
    
    # Check AUR updates separately if pikaur is available
    local aur_count=0
    if command -v pikaur &> /dev/null; then
        aur_count=$(pikaur -Qua 2>/dev/null | wc -l || echo 0)
    fi
    
    local total_outdated=$((outdated_count + aur_count))
    
    if [[ $total_outdated -ge $PACMAN_CRITICAL_THRESHOLD ]]; then
        anomalies+=("CRITICAL: $total_outdated outdated packages ($outdated_count official, $aur_count AUR)")
    elif [[ $total_outdated -ge $PACMAN_WARN_THRESHOLD ]]; then
        warnings+=("$total_outdated outdated packages ($outdated_count official, $aur_count AUR)")
    elif [[ $total_outdated -gt 0 ]]; then
        info+=("$total_outdated outdated packages ($outdated_count official, $aur_count AUR)")
    fi
}

# Check 2: Failed systemd services
check_services() {
    local failed_services
    failed_services=$(systemctl --failed --no-pager --no-legend 2>/dev/null | awk '$1 != "" {print $1}' || echo "")
    
    if [[ -n "$failed_services" ]]; then
        local count=$(echo "$failed_services" | grep -c . || echo 0)
        if [[ $count -gt 0 ]]; then
            anomalies+=("FAILED SERVICES ($count): $(echo "$failed_services" | tr '\n' ' ')")
        fi
    fi
}

# Check 3: Disk space usage
check_disk_space() {
    local critical_mounts=()
    
    while IFS= read -r line; do
        local mount_point=$(echo "$line" | awk '{print $6}')
        local usage=$(echo "$line" | awk '{print $5}' | sed 's/%//')
        local filesystem=$(echo "$line" | awk '{print $1}')
        
        # Skip special filesystems
        if [[ "$filesystem" =~ ^(tmpfs|devtmpfs|udev|overlay) ]]; then
            continue
        fi
        
        if [[ $usage -ge 95 ]]; then
            anomalies+=("DISK CRITICAL: $mount_point at ${usage}%")
        elif [[ $usage -ge $DISK_WARN_THRESHOLD ]]; then
            warnings+=("Disk: $mount_point at ${usage}%")
        fi
    done < <(df -h 2>/dev/null | tail -n +2)
}

# Check 4: Pacman lock file (indicates hung package operation)
check_pacman_lock() {
    if [[ -f /var/lib/pacman/db.lck ]]; then
        anomalies+=("Pacman database locked (stale lock file or operation in progress)")
    fi
}

# Check 5: Journal errors (last hour)
check_journal_errors() {
    if command -v journalctl &> /dev/null; then
        local error_count
        error_count=$(journalctl -p err -S "1 hour ago" --no-pager --quiet 2>/dev/null | wc -l || echo 0)
        
        if [[ $error_count -gt 50 ]]; then
            anomalies+=("High error rate: $error_count journal errors in last hour")
        elif [[ $error_count -gt 10 ]]; then
            warnings+=("$error_count journal errors in last hour")
        fi
    fi
}

# Check 6: Kernel mismatch (running vs installed)
check_kernel() {
    local running_kernel
    local installed_kernel
    
    running_kernel=$(uname -r)
    installed_kernel=$(pacman -Q linux 2>/dev/null | awk '{print $2}' || echo "unknown")
    
    # Extract version numbers for comparison (rough check)
    if [[ "$installed_kernel" != "unknown" ]]; then
        local running_ver=$(echo "$running_kernel" | cut -d'-' -f1)
        local installed_ver=$(echo "$installed_kernel" | cut -d'-' -f1)
        
        if [[ "$running_ver" != "$installed_ver" ]]; then
            warnings+=("Kernel mismatch: running $running_kernel, installed $installed_kernel (reboot needed)")
        fi
    fi
}

# Check 7: System load
check_load() {
    local load_avg
    local cpu_count
    
    load_avg=$(uptime | awk -F'load average:' '{print $2}' | awk -F',' '{print $1}' | xargs)
    cpu_count=$(nproc)
    
    # Convert load to integer for comparison (multiply by 100)
    # Use awk instead of bc for better portability
    local load_int=$(echo "$load_avg" | awk '{printf "%d", $1 * 100}')
    local threshold_int=$((cpu_count * 200))  # 2x CPU count
    
    if [[ $load_int -gt $threshold_int ]]; then
        warnings+=("High load average: $load_avg on $cpu_count CPUs")
    fi
}

# Check 8: Memory pressure
check_memory() {
    local mem_info
    mem_info=$(free | grep Mem)
    
    local total=$(echo "$mem_info" | awk '{print $2}')
    local available=$(echo "$mem_info" | awk '{print $7}')
    
    local usage_pct=$((100 - (available * 100 / total)))
    
    if [[ $usage_pct -gt 95 ]]; then
        anomalies+=("Memory critical: ${usage_pct}% used")
    elif [[ $usage_pct -gt 85 ]]; then
        warnings+=("Memory pressure: ${usage_pct}% used")
    fi
}

# Run all checks
check_packages
check_services
check_disk_space
check_pacman_lock
check_journal_errors
check_kernel
check_load
check_memory

# Output results
if $output_json; then
    # JSON output for machine parsing
    echo "{"
    echo "  \"timestamp\": \"$(date -Iseconds)\","
    echo "  \"host\": \"$(cat /etc/hostname 2>/dev/null || echo 'unknown')\","
    echo "  \"status\": \"$([ ${#anomalies[@]} -eq 0 ] && echo "healthy" || echo "degraded")\","
    echo "  \"anomalies\": ["
    for i in "${!anomalies[@]}"; do
        echo -n "    \"${anomalies[$i]}\""
        [[ $i -lt $((${#anomalies[@]} - 1)) ]] && echo "," || echo ""
    done
    echo "  ],"
    echo "  \"warnings\": ["
    for i in "${!warnings[@]}"; do
        echo -n "    \"${warnings[$i]}\""
        [[ $i -lt $((${#warnings[@]} - 1)) ]] && echo "," || echo ""
    done
    echo "  ],"
    echo "  \"info\": ["
    for i in "${!info[@]}"; do
        echo -n "    \"${info[$i]}\""
        [[ $i -lt $((${#info[@]} - 1)) ]] && echo "," || echo ""
    done
    echo "  ]"
    echo "}"
elif $output_brief; then
    # Ultra-brief output for regular heartbeats
    if [[ ${#anomalies[@]} -eq 0 && ${#warnings[@]} -eq 0 ]]; then
        echo "OK"
    else
        [[ ${#anomalies[@]} -gt 0 ]] && printf "⚠ %s\n" "${anomalies[@]}"
        [[ ${#warnings[@]} -gt 0 ]] && printf "⚡ %s\n" "${warnings[@]}"
    fi
else
    # Human-readable output
    host=$(cat /etc/hostname 2>/dev/null || echo 'unknown')
    if [[ ${#anomalies[@]} -eq 0 && ${#warnings[@]} -eq 0 && ${#info[@]} -eq 0 ]]; then
        echo "✓ System healthy ($host @ $(date '+%Y-%m-%d %H:%M'))"
        exit 0
    fi
    
    echo "=== System Health: $host @ $(date '+%Y-%m-%d %H:%M') ==="
    echo ""
    
    if [[ ${#anomalies[@]} -gt 0 ]]; then
        echo "🔴 ANOMALIES:"
        printf '  • %s\n' "${anomalies[@]}"
        echo ""
    fi
    
    if [[ ${#warnings[@]} -gt 0 ]]; then
        echo "⚠️  WARNINGS:"
        printf '  • %s\n' "${warnings[@]}"
        echo ""
    fi
    
    if [[ ${#info[@]} -gt 0 ]]; then
        echo "ℹ️  INFO:"
        printf '  • %s\n' "${info[@]}"
        echo ""
    fi
    
    # Return non-zero if there are anomalies
    [[ ${#anomalies[@]} -gt 0 ]] && exit 1
fi

exit 0
