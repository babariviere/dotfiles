#!/usr/bin/env bash

THRESHOLD=15 # percent
export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
export XAUTHORITY=$HOME/.Xauthority

lock_path='/tmp/batmon.lock'

echo "lock" > $lock_path

acpi_path=$(find /sys/class/power_supply/ -name 'BAT*' | head -1)
charge_now=$(cat "$acpi_path/charge_now")
charge_full=$(cat "$acpi_path/charge_full")
charge_status=$(cat "$acpi_path/status")
charge_percent=$(printf '%.0f' $(echo "$charge_now / $charge_full * 100" | bc -l))
message="Battery running at $charge_percent%!"

if [[ $charge_status == 'Discharging' ]] && [[ $charge_percent -le $THRESHOLD ]]; then
  /usr/bin/notify-send -a "battery monitor" -u critical "Low battery" "$message"
fi

rm -f $lock_path
