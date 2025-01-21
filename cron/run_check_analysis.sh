#!/bin/bash

printf "\n\n"
date

unset LANG
basedir=$WORKDIR_BASE/nwprun
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/icon_2I/enda_dia

# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/arki_tools.sh

set -e
set -x

mkdir -p $WORKDIR
cd $WORKDIR

# Configure eccodes definitions
export ECCODES_DEFINITION_PATH=$ECCODES_DEFINITIONS_DWD:$ECCODES_DEFINITIONS_BASE

# Check analysis delay and cold restart
$SIMC_TOOLS python3 $basedir/ecflow/script_python3/check_analysis.py \
                    --folder $LETKF_ARCHIVE --hours 12 --threshold 2.0 --delay_alert 4

# Send email if alert_message.txt is present
if [ -s "alert_message.txt" ]; then
    EMAIL_SUBJECT="ALERT: ICON-2I-KENDA issue detected"
    echo "$(cat alert_message.txt)" | /usr/bin/mailx -s "$EMAIL_SUBJECT" -S sendwait ${EMAIL_ADDRESS[@]}
    echo $?

    # Remove file
    rm -f alert_message.txt
fi

exit 0
