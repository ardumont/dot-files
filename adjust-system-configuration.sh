#!/bin/bash -x

# Use: $0
# A script to adjust environment settings to match my needs

#### Suspend on lid close event

ACPI_SUPPORT_FILE=/etc/default/acpi-support
# Expressions to look for/replace
EXPRESSION="LID_SLEEP=true"
EXPRESSION_TO_LOOK_FOR="#$EXPRESSION"

# I no longer used desktop environment (so no power management, etc...) but windows manager (like stumpwm, xmonad, etc...)
# On my actual system, the LID_SLEEP variable in $ACPI_SUPPORT_FILE is commented so no suspension takes place when the laptop lid is closed
# This will modify such variable if need be

if [ -f $ACPI_SUPPORT_FILE ]; then
    set +e # deactivate fail-fast

    grep -q "$EXPRESSION_TO_LOOK_FOR" $ACPI_SUPPORT_FILE
    [ $? -eq 0 ] && sudo sed 's/$EXPRESSION_TO_LOOK_FOR/$EXPRESSION/g' -i $ACPI_SUPPORT_FILE
fi

exit 0
