case "$HOSTNAME" in
  *.metarpa | maial* ) # Arpae
# General
    WORKDIR=$WORK/$NWPCONF
    LOGDIR=$SCRATCH/log
# Arkimet
    ARKI_DIR=/fs/archive/arkimet/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=arki_importer
    ARKI_IMPDIR=/fs/work/arkimet/inbound
    ARKI_IMPROOT=$ARKI_IMPDIR
    ARKI_URL=http://maialtest.metarpa:8090/dataset
    ARKI_USE_INOTIFY=Y
# Radar
    RADAR_MOSAICODIR=$HOME/prelhn/Composito
    RADAR_LHNDIR=$HOME/prelhn/bufr2grib-RUC
    RADAR_DT=10
    ;;
  lami* ) # Cineca cloud
# General
    WORKDIR=$HOME/$NWPCONF
    LOGDIR=$HOME/log
# Arkimet
    ARKI_DIR=/arkimet/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=arki_importer
    ARKI_IMPDIR=~arki-imp/arki-imp/generic
    ARKI_IMPROOT=~arki-imp/arki-imp
    ARKI_URL=/arkimet/arkimet
    ARKI_USE_INOTIFY=Y
    IMPORT_SIGNAL_METHOD=psql
    IMPORT_SIGNAL_ARGS="-h localhost -d import -U logsim"
    ;;
  node??? ) # Cineca HPC galileo
# General
    WORKDIR=$CINECA_SCRATCH/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
# Arkimet
    ARKI_SCAN_METHOD=remote_arki_importer
#
#
    ARKI_IMPDIR=~arki-imp/arki-imp/generic
#
#
    IMPORT_SIGNAL_METHOD=curl
#
#
    CINECA_ARCHIVE_PRE=/gpfs/meteo/lm/galileo/auto/archive/PROD
    ;;
  r????????? ) # Cineca HPC marconi
# General
    WORKDIR=$CINECA_SCRATCH/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
# Arkimet
    ARKI_SCAN_METHOD=remote_arki_importer
#
#
    ARKI_IMPDIR=~arki-imp/arki-imp/generic
#
#
    IMPORT_SIGNAL_METHOD=curl
#
#
    CINECA_ARCHIVE_PRE=/marconi_meteo/lm/marconi/auto/archive/PROD
    ;;
esac

# GRIB coding
GRIB_CENTER=80
BUFR_ARKI_DS="$ARKI_URL/cnmc_amdar146 $ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship $ARKI_URL/cnmc_wprof"

# ensemble prediction moved to nwpconf/cosmo_model
#ENS_MODE=.FALSE.
#if [ -n "$ENS_TOTAL_MEMB" -a -n "$ENS_MEMB" ]; then
#    ENS_MODE=.TRUE.
#fi

