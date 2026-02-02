# System Health Monitor

Arch Linux system health monitoring tool that generates concise status summaries optimized for LLM consumption.

## Features

- **Package Updates**: Monitors outdated packages (both official repos and AUR via pikaur)
- **Failed Services**: Detects failed systemd services
- **Disk Space**: Monitors filesystem usage
- **Pacman Lock**: Detects stale package manager locks
- **Journal Errors**: Tracks error rate in systemd journal
- **Kernel Mismatch**: Detects when running kernel differs from installed (reboot needed)
- **System Load**: Monitors CPU load average
- **Memory Pressure**: Tracks memory usage

## Usage

```bash
# Standard output (human-readable)
syshealth

# Brief output (for heartbeats)
syshealth --brief

# JSON output (for machine parsing)
syshealth --json
```

## Output Modes

### Standard (default)
Human-readable format with emoji indicators:
- 🔴 ANOMALIES: Critical issues requiring immediate attention
- ⚠️  WARNINGS: Issues that should be addressed soon
- ℹ️  INFO: Informational notices

Returns exit code 0 if healthy, 1 if anomalies present.

### Brief (`--brief`)
Minimal output for regular automated checks:
- `OK` if everything is healthy
- `⚠ [issue]` for anomalies
- `⚡ [issue]` for warnings

### JSON (`--json`)
Machine-parseable JSON format with timestamp, status, and categorized issues.

## Signal-to-Noise Optimization

The tool is designed to minimize token burn in automated monitoring:

1. **Anomaly-first**: Only reports deviations from healthy state
2. **Tiered severity**: Separates critical issues from warnings
3. **Aggregated metrics**: Counts instead of full lists where appropriate
4. **Concise descriptions**: Brief, parseable messages

## Thresholds

Default thresholds (configurable in `defaults/main.yml`):
- Disk space: 80% usage (warning), 95% (critical)
- Outdated packages: 20 (warning), 50 (critical)
- Journal errors: 10/hour (warning), 50/hour (critical)
- Memory usage: 85% (warning), 95% (critical)
- Load average: 2x CPU count (warning)

## Installation

Installed via Ansible (dotsible):

```bash
dotsible --tags syshealth
```

This creates a symlink from the dotfiles repo to `~/.local/bin/syshealth`.

## Integration

Designed for hourly automated checks via HEARTBEAT.md with the following logic:
- Report anomalies immediately
- Weekly summary of overall health trends
- Stay silent during normal operation

## Dependencies

- `bash`
- `systemd` (for service/journal checks)
- `pacman` (Arch package manager)
- `pikaur` (optional, for AUR updates)
- `checkupdates` (from pacman-contrib)
- `bc` (for numeric comparisons)

All dependencies are typically present on standard Arch Linux installations.
