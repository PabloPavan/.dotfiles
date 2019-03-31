#!/bin/bash

wifi=$(iw wlp8s0 info | grep -w ssid | sed 's/ssid//' | sed 's/[[:blank:]]//g')

echo Wifi: $wifi

exit 0