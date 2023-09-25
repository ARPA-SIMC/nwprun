MODEL_BASE=/g100_work/smr_prod/srcintel/icon-2.6.5.1
MODEL_BIN=$MODEL_BASE/bin/icon
ECRAD_DATA=$MODEL_BASE/data
MODEL_STATIC=$WORKDIR_BASE/data/icon
MODEL_PRE_BINDIR=/ind2/meteo/a07smr03/icon/build/icontools-2.5.0.intel/icontools
PARENTMODEL_DATADIR=$WORKDIR/input/data

# Working directories for observations
BUFR_WORKDIR=$WORKDIR/bufr
MODEL_LHN_WORKDIR=$WORKDIR/lhn
HDF5_WORKDIR=$WORKDIR/radar_vol

# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_DIR/gts_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_DIR/gts_bufr_noconv
BUFR_ARKI_DS_RADARVOL=$ARKI_DIR/radar_vol
ARKI_LHN_DS=$ARKI_DIR/radar_sri
MODEL_LHN_DT=600
FREQ_FILE_BUFR=1 #improve

# Working directories for ICON and its preprocessing
MODEL_PRE_WORKDIR=$WORKDIR/preicon
MODEL_PRE_DATADIR=$WORKDIR/preicon/data
MODEL_WORKDIR=$WORKDIR/icon
MODEL_DATADIR=$WORKDIR/icon/data

# ICON-2I domain and grid files
DOMAIN=ICON_REG3_DOM01	
LOCALGRID=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.nc
LOCALGRID_PARENT=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.parent.nc
LOCALGRID_EXTERNAL=$MODEL_STATIC/domain_$DOMAIN/external_parameter_icon_${DOMAIN}_tiles.nc

# Specifics for IAU initialization
ASS_CYCLE_LENGTH=1
DT_IAU=600

# eccodes definitions
ecd_dwd=/ind2/meteo/a07smr03/icon/definitions/definitions.edzw-2.21.0-1
ecd_cin=/ind2/meteo/a07smr03/icon/definitions/definitions.cineca-2.21.0
ECCODES_DEFINITION_PATH=${ecd_dwd}:${ecd_cin}
unset ecd_dwd ecd_cin

# Generating process
MODEL_ASSIM_GP=61
MODEL_ASSIM_INC_GP=61
MODEL_FCAST_GP=62
MODEL_FCRUC_GP=64
MODEL_FCENS_GP=65
MODEL_INTER_GP=63
MODEL_ARKI_TIMERANGE_ASSIM="origin:GRIB2,,,0,,$MODEL_ASSIM_GP"
MODEL_ARKI_TIMERANGE_ASSIM_INC="origin:GRIB2,,,201,,$MODEL_ASSIM_INC_GP"
MODEL_ARKI_TIMERANGE_FCAST="origin:GRIB2,,,2,,$MODEL_FCAST"
ANA_EXT=0000
ANA_DET_EXT=.det

# Per gli esperimenti
STOP_ON_FAIL=Y

