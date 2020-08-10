#!/usr/bin/env bash
if timeout --signal KILL 2m offlineimap -q; then
    echo "Fetch successful!"
else
    echo "OfflineIMAP timed out: KILL and remove locks"
    rm ~/.offlineimap/*.lock
fi
