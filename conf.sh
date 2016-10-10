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
    ARKI_URL=http://localhost:8090
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
BUFR_ARKI_DS="$ARKI_URL/dataset/cnmc_acars $ARKI_URL/dataset/cnmc_airep $ARKI_URL/dataset/cnmc_amdar $ARKI_URL/dataset/cnmc_buoy $ARKI_URL/dataset/cnmc_pilot $ARKI_URL/dataset/cnmc_ship11 $ARKI_URL/dataset/cnmc_ship13 $ARKI_URL/dataset/cnmc_ship9 $ARKI_URL/dataset/cnmc_synop1 $ARKI_URL/dataset/cnmc_synop3 $ARKI_URL/dataset/cnmc_temp $ARKI_URL/dataset/cnmc_tempship $ARKI_URL/dataset/cnmc_wprof"

# ensemble prediction
ENS_MODE=.FALSE.
if [ -n "$ENS_TOTAL_MEMB" ]; then
    if [ "$ENS_TOTAL_MEMB" -gt 1 -a -n "$ENS_MEMB" ]; then
        ENS_MODE=.TRUE.
    fi
fi

