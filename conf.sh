# General
if [ -n "$OPE" ]; then
  WORKDIR=$OPE/$NWPCONF
else
  WORKDIR=$HOME/$NWPCONF
fi
if [ -n "$SCRATCH" ]; then
  LOGDIR=$SCRATCH/log
else
  LOGDIR=$HOME/log
fi

# ARKIMET
ARKI_DIR=/arkimet/arkimet
ARKI_CONF=$ARKI_DIR/config
ARKI_SCAN_METHOD=arki_importer
ARKI_IMPDIR=~arki-imp/arki-imp
#
ARKI_USE_INOTIFY=Y

# GRIB coding
GRIB_CENTER=80

