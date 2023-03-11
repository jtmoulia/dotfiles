#!/usr/bin/env bash
kill -9 $(pgrep offlineimap)
rm ~/.offlineimap/*.lock
