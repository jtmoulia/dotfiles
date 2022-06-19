#!/usr/bin/env bash

if [ $# -eq 0 ]; then
    echo "Missing log group argument (e.g. SLHSProd)"
    echo "Usage: alog GROUP [STREAM] [SINCE]"
    exit 1
elif [ $# -eq 1 ]; then
    aws logs tail "$1-logs" --follow
elif [ $# -eq 2 ]; then
    aws logs tail "$1-logs" --log-stream-names "$2" --follow
elif [ $# -eq 3 ]; then
    aws logs tail "$1-logs" --log-stream-names "$2" --since "$3" --follow
fi
