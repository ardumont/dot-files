#!/bin/bash -x
set +e # deactivate fail-fast

# Use: $0
# A script to adjust environment settings to match my needs

#### Suspend on lid close event

# I no longer used desktop environment (so no power management, etc...) but windows manager (like stumpwm, xmonad, etc...)
# On my actual system, the LID_SLEEP variable in /etc/default/acpi-support is commented so no suspension takes place when the laptop lid is closed
# This will modify such variable if need be

if [ -f /etc/default/acpi-support ]; then
    grep -q "#LID_SLEEP=true" /etc/default/acpi-support
    [ $? -eq 0 ] && sudo sed 's/#LID_SLEEP=true/LID_SLEEP=true/g' -i /etc/default/acpi-support
fi
