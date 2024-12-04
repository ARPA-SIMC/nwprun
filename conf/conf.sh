# local configuration
[ -f "$HOME/.nwpconf" ] && source $HOME/.nwpconf

case "$HPC_SYSTEM" in
  *.metarpa | maial* | nodo* ) # Arpae
# General
    WORKDIR=$WORK/$NWPCONF
    LOGDIR=$WORKDIR_BASE/log
    ARKI_DIR=$WORKDIR_BASE/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=configured_importer
    IMPORT_SIGNAL_METHOD=filesystem
    IMPORT_SIGNAL_BASE=$ARKI_DIR/import_signal
    ARKI_IMPROOT=$WORKDIR_BASE/import
    ARKI_IMPDIR=$ARKI_IMPROOT
    GRIB_API_EDZW=$HOME/srcgnu/grib_api_edzw
    SIMC_TOOLS=
    SIMC_SITE=Y
    ecflow_client=ecflow_client
# Radar
    RADAR_MOSAICODIR=$HOME/radarsri2grib
    RADAR_LHNDIR=$HOME/radarsri2grib
    RADAR_DT=10
    ;;
  lami* ) # Cineca cloud
# General
    WORKDIR=/arkimet/arkimet/$NWPCONF
    LOGDIR=$HOME/log
    ARKI_DIR=/arkimet/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=arki_importer
    IMPORT_SIGNAL_METHOD=psql
    ARKI_IMPROOT=~arki-imp/arki-imp
    ARKI_IMPDIR=$ARKI_IMPROOT
    ARKI_USE_INOTIFY=Y
    ;;
  g100 ) # Cineca HPC galileo 100
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    if [ -n "$WORKDIR_RELOC" ]; then
	    WORKDIR=$WORKDIR_RELOC/$NWPCONF
    fi
    LOGDIR=$WORKDIR_BASE/log
    ARKI_DIR=$WORKDIR_BASE/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=configured_importer
    IMPORT_SIGNAL_METHOD=filesystem
    IMPORT_SIGNAL_BASE=$ARKI_DIR/import_signal
    ARKI_IMPROOT=$WORKDIR_BASE/import
    ARKI_IMPDIR=$ARKI_IMPROOT
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    SIMC_TOOLS="singularity exec -B /ind2/meteo -B /ind2/meteoarch $WORKDIR_BASE/simctools_nwprun_r8.sif"
    CINECA_ARCHIVE_POST=$WORKDIR_BASE/arkimet/archive
    CINECA_RUN=/ind2/meteo/a07smr01/LM_WorkDir/g100/lm/PROD/work
    CINECA_RUN_ICON=/ind2/meteo/a07smr01/ICON_WorkDir/PROD
    CINECA_UPLDIR=/ind2/meteoarch/a07smr01/arch_lm/g100/auto/download/OPE/AM/LAMI/IFS
    ecflow_client="$WORKDIR_BASE/nwprun/ecflow/ec_wrap ecflow_client"
    ;;
  leonardo ) # Cineca HPC leonardo
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    LOGDIR=$WORKDIR_BASE/log
    ARKI_DIR=$WORKDIR_BASE/arkimet
    ARKI_CONF=$ARKI_DIR/config
    ARKI_SCAN_METHOD=configured_importer
    IMPORT_SIGNAL_METHOD=filesystem
    IMPORT_SIGNAL_BASE=$ARKI_DIR/import_signal
    ARKI_IMPROOT=$WORKDIR_BASE/import
    ARKI_IMPDIR=$ARKI_IMPROOT
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    SIMC_TOOLS="singularity exec -B $WORK -B /leonardo_scratch $WORKDIR_BASE/simctools_nwprun_r8.sif"
    CINECA_ARCHIVE_POST=$WORKDIR_BASE/arkimet/archive
    CINECA_RUN=/leonardo_work/smr_prod/a07smr01/LM_WorkDir/leonardo/lm/PROD/work
    CINECA_RUN_ICON=/leonardo_work/smr_prod/a07smr01/ICON_WorkDir/PROD
    CINECA_UPLDIR=/meteo_arch/a07smr01/arch_lm/leonardo/auto/download/OPE/AM/AM/LAMI/IFS
    ecflow_client="$WORKDIR_BASE/nwprun/ecflow/ec_wrap ecflow_client"
    ;;

  marconi ) # Cineca HPC marconi
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    ;;
esac

LOG4C_APPENDER=stderr
LOG4C_PRIORITY=warning
# GRIB coding and BUFR observations
GRIB_CENTER=80
#BUFR_ARKI_DS="$ARKI_URL/cnmc_amdar146 $ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship $ARKI_URL/cnmc_wprof"
#BUFR_ARKI_DS="$ARKI_URL/cnmc_amdar146 $ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship"
BUFR_ARKI_DS_CONV="$ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship"
BUFR_ARKI_DS_NOCONV="$ARKI_URL/cnmc_amdar146"
FREQ_FILE_BUFR=3


