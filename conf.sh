case "$HOSTNAME" in
  *.metarpa | maialinux ) # Arpae
# General
    WORKDIR=$OPE/$NWPCONF
    LOGDIR=$SCRATCH/log
# Arkimet
    ARKI_DIR=$OPE/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=arki_importer
    ARKI_IMPDIR=$ARKI_DIR/inbound
    ARKI_URL=http://maialinux.metarpa:8090
    ARKI_USE_INOTIFY=Y
# Radar
    RADAR_MOSAICODIR=$HOME/prelhn/Composito
    RADAR_LHNDIR=$HOME/prelhn/bufr2grib-RUC
    RADAR_DT=10
    ;;
  *.novalocal ) # Cineca cloud
# General
    WORKDIR=$HOME/$NWPCONF
    LOGDIR=$HOME/log
# Arkimet
    ARKI_DIR=/arkimet/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=arki_importer
    ARKI_IMPDIR=~arki-imp/arki-imp
#
    ARKI_USE_INOTIFY=Y
    ;;
  node* ) # Cineca HPC
# General
    WORKDIR=$CINECA_SCRATCH/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
# Arkimet
    ARKI_SCAN_METHOD=remote_arki_importer
#
    ARKI_IMPDIR=~arki-imp/arki-imp
#
    ;;
esac

# GRIB coding
GRIB_CENTER=80

