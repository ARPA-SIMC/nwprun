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
ARKI_DIR=$OPE/arkimet
ARKI_CONF=$ARKI_DIR/config
ARKI_SCAN_METHOD=arki_importer
ARKI_IMPDIR=$ARKI_DIR/inbound
ARKI_URL=http://maialinux.metarpa:8090
ARKI_USE_INOTIFY=Y

# GRIB coding
GRIB_CENTER=80

