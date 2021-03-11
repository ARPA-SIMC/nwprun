#!/bin/bash
# script for downloading the new bufr provided by COMET (CNMCA) since 2021

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

dl_ftp() {

    file="${FTPDIR}BUFR0001${DATETIME}.DAT"
    rm -f $file
    ncftpget -V $ncftpauth . $file || true
    if [ -f "$file" ]; then
	putarki_configured_archive $1 $file
	return 0
    fi
    return 1

}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/GTS_BUFR

set -x
set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arki_tools.sh
. $NWPCONFBINDIR/nwpwait.sh
# end of setup


# improve
ncftpauth="-f $basedir/.auth/bufr_ruc_get.cfg"

if [ -n "$1" ]; then # interactive run
    GTS_BUFR_WORKDIR=${GTS_BUFR_WORKDIR}_interactive
    DATETIME=$1
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}
else # automatic run
    nonunique_exit
    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1

    restore_state gts_bufr_get.state || touch $NWPCONFDIR/$NWPCONF/gts_bufr_get.state
    if [ -z "$DATETIME" ]; then # set minimum datetime
	DATETIME=`date -u --date '1 day ago' '+%Y%m%d12'`
    else # increment datetime
	DATETIME=`datetime_add $DATETIME $GTS_BUFR_STEP`
    fi
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}

# wait before querying the server
    NWPWAITSOLAR_SAVE=$NWPWAITSOLAR
    NWPWAITSOLAR=$NWPWAITSOLAR_RUN
    NWPWAITWAIT_SAVE=$NWPWAITWAIT
    unset NWPWAITWAIT
    nwpwait_setup
    nwpwait_wait && exit 0 # too early, try next time

# wait until reasonable
    NWPWAITSOLAR=$NWPWAITSOLAR_SAVE
    NWPWAITWAIT=$NWPWAITWAIT_SAVE
fi

safe_rm_rf $GTS_BUFR_WORKDIR
mkdir -p $GTS_BUFR_WORKDIR
cd $GTS_BUFR_WORKDIR

dirname=gts_bufr
putarki_configured_setup $dirname "reftime=$DATETIME" "format=bufr" "signal=gts_bufr" "configext=test"
nwpwait_setup

while true; do
    dl_ftp $dirname && break
    nwpwait_wait || break
done

putarki_configured_end $dirname

if [ -n "$1" ]; then # interactive run
    :
else # automatic run
    save_state gts_bufr_get.state DATETIME
fi
