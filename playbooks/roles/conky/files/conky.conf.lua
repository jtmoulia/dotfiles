--[[
Conky, a system monitor, based on torsmo

Any original torsmo code is licensed under the BSD license

All code written since the fork of torsmo is licensed under the GPL

Please see COPYING for details

Copyright (c) 2004, Hannu Saransaari and Lauri Hakkarainen
Copyright (c) 2005-2019 Brenden Matthews, Philip Kovacs, et. al. (see AUTHORS)
All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
]]

conky.config = {
    alignment = 'top_right',
    background = false,
    border_width = 1,
    cpu_avg_samples = 2,
    default_color = 'white',
    default_outline_color = 'white',
    default_shade_color = 'black',
    draw_borders = true,
    draw_graph_borders = true,
    draw_outline = false,
    draw_shades = true,
    double_buffer = true,
    use_xft = true,
    font = 'DejaVu Sans Mono:size=14',
    gap_x = 12,
    gap_y = 48,
    minimum_height = 5,
    minimum_width = 5,
    net_avg_samples = 2,
    no_buffers = true,
    out_to_console = false,
    out_to_ncurses = false,
    out_to_stderr = false,
    out_to_x = true,
    extra_newline = false,
    own_window_class = 'Conky',
    own_window_type = 'normal',
    own_window_transparent = true,
    own_window_argb_visual = true,
    own_window_argb_value = 0,
    own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
    stippled_borders = 0,
    update_interval = 1.0,
    uppercase = false,
    use_spacer = 'none',
    show_graph_scale = false,
    show_graph_range = false
}

conky.text = [[
${color grey}System:$color $alignr $sysname $kernel $machine
${color grey}Account:$color $alignr ${execi 100000 whoami}@$nodename
${color grey}Time:$color $alignr $time
${color grey}Uptime:$color $alignr $uptime
$hr
${color grey}RAM Usage:$color $alignr $mem/$memmax $memperc%
  ${membar 4}
${color grey}Swap Usage:$color $alignr $swap/$swapmax - $swapperc%
  ${swapbar 4}
$hr
${color grey}CPU Frequency (in GHz):$color $alignr $freq_g
${color grey}CPU Usage:$color $cpu% ${cpubar 4}
 ${color grey}CPU 1:$color ${cpu cpu1}% ${cpubar cpu1 4}
 ${color grey}CPU 2:$color ${cpu cpu2}% ${cpubar cpu2 4}
 ${color grey}CPU 3:$color ${cpu cpu3}% ${cpubar cpu3 4}
 ${color grey}CPU 4:$color ${cpu cpu4}% ${cpubar cpu4 4}
 ${cpugraph}
${color grey}Processes:$color $processes  ${color grey}Running:$color $running_processes
$hr
${color grey}File systems:
 / $color${fs_used /}/${fs_size /} ${fs_bar 6 /}
${color grey}Networking:
 ${color grey}IP Address (wlp3s0):$color $alignr ${execi 600 ip addr show wlp3s0 | perl -ne 'print "$1\n" if /inet (.*)\//'}
 ${color grey}Up / Down:$color ${upspeed wlp3s0} $alignr ${downspeed wlp3s0}
 ${upspeedgraph wlp3s0}
 ${downspeedgraph wlp3s0}
${color grey}Battery:$color ${battery_short BAT1} ${battery_bar 4 BAT1}
$hr
${color grey}Process Name     PID    CPU%   MEM%
${color lightgrey} ${top name 1} ${top pid 1} ${top cpu 1} ${top mem 1}
${color lightgrey} ${top name 2} ${top pid 2} ${top cpu 2} ${top mem 2}
${color lightgrey} ${top name 3} ${top pid 3} ${top cpu 3} ${top mem 3}
${color lightgrey} ${top name 4} ${top pid 4} ${top cpu 4} ${top mem 4}
${color lightgrey} ${top name 5} ${top pid 5} ${top cpu 5} ${top mem 5}
${color grey}Failed systemd units:
${color lightgrey}${execi 300 systemctl list-units --failed --full --all --plain --no-legend | perl -ne 'print " $1\n" if /^(.*) +loaded.*$/'}
]]
