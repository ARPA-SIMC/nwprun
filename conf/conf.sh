# local configuration
[ -f "$HOME/.nwpconf" ] && source $HOME/.nwpconf

case "$HPC_SYSTEM" in
  *.metarpa | maial* | nodo* ) # Arpae
# General
    WORKDIR=$WORK/$NWPCONF
    LOGDIR=$WORK/log
    IMPORT_SIGNAL_BASE=$ARKI_DIR/import_signal
    LOGSIM_SIGNAL_SYNC=Y
    GRIB_API_EDZW=$HOME/srcgnu/grib_api_edzw
# Radar
    RADAR_MOSAICODIR=$HOME/radarsri2grib
    RADAR_LHNDIR=$HOME/radarsri2grib
    RADAR_DT=10
    ;;
  lami* ) # Cineca cloud
# General
    WORKDIR=/arkimet/arkimet/$NWPCONF
    LOGDIR=$HOME/log
    ;;
  galileo ) # Cineca HPC galileo
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    ;;
  meucci ) # Cineca HPC meucci
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    LOGDIR=$WORKDIR_BASE/log
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    EXTRA_MPIRUN="-genv I_MPI_FABRICS=shm:dapl -genv I_MPI_DAPL_SCALABLE_PROGRESS=1 -genv DAT_OVERRIDE=$NWPCONFDIR/$NWPCONF/dat.conf -genv DAPL_ACK_RETRY=7 -genv DAPL_ACK_TIMER=20 -genv DAPL_CM_ROUTE_TIMEOUT_MS=20000 -genv DAPL_CM_ARP_TIMEOUT_MS=10000 -genv DAPL_UCM_REP_TIME=2000 -genv DAPL_UCM_RTU_TIME=2000 -genv DAPL_UCM_RETRY=7 -genv DAPL_UCM_CQ_SIZE=2000 -genv DAPL_UCM_QP_SIZE=2000"
    EXTRA_MPIRUN_LETKF="-genv I_MPI_FABRICS=shm:tcp"
    ;;
  g100 ) # Cineca HPC galileo 100
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    if [ -n "$WORKDIR_RELOC" ]; then
	    WORKDIR=$WORKDIR_RELOC/$NWPCONF
    fi
    LOGDIR=$WORKDIR_BASE/log
    IMPORT_SIGNAL_BASE=$ARKI_DIR/import_signal
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    ;;
  marconi ) # Cineca HPC marconi
# General
    WORKDIR=$WORKDIR_BASE/$NWPCONF
    LOGDIR=$CINECA_SCRATCH/log
    GRIB_API_EDZW=$WORKDIR_BASE/grib_api_edzw
    ;;
esac

# GRIB coding and BUFR observations
GRIB_CENTER=80
#BUFR_ARKI_DS="$ARKI_URL/cnmc_amdar146 $ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship $ARKI_URL/cnmc_wprof"
#BUFR_ARKI_DS="$ARKI_URL/cnmc_amdar146 $ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship"
BUFR_ARKI_DS_CONV="$ARKI_URL/cnmc_buoy $ARKI_URL/cnmc_pilot $ARKI_URL/cnmc_ship11 $ARKI_URL/cnmc_ship13 $ARKI_URL/cnmc_ship9 $ARKI_URL/cnmc_synop1 $ARKI_URL/cnmc_synop3 $ARKI_URL/cnmc_temp $ARKI_URL/cnmc_tempship"
BUFR_ARKI_DS_NOCONV="$ARKI_URL/cnmc_amdar146"
FREQ_FILE_BUFR=3


