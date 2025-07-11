# Model environment variables
MODEL_BASE=$WORKDIR_BASE/srcintel/icon_2025-04-1
MODEL_BIN=$MODEL_BASE/bin/icon
ECRAD_DATA=$MODEL_BASE/data
MODEL_STATIC=$WORKDIR_BASE/data/icon
MODEL_PRE_BINDIR=$WORKDIR_BASE/srcintel/icontools-2.5.0/icontools
PARENTMODEL_DATADIR=$WORKDIR/input/data

# Working directories for observations
BUFR_WORKDIR=$WORKDIR/bufr
MODEL_LHN_WORKDIR=$WORKDIR/lhn
HDF5_WORKDIR=$WORKDIR/radar_vol

# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_DIR/gts_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_DIR/gts_bufr_noconv
BUFR_ARKI_DS_RADARVOL=$ARKI_DIR/radar_vol
ARKI_LHN_DS=$ARKI_DIR/icon_2I_radar
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

# Time step
TIME_STEP=20

# Specifics for IAU initialization
ASS_CYCLE_LENGTH=1
DT_IAU=600

# eccodes definitions
ECCODES_DEFINITION_PATH=$ECCODES_DEFINITIONS_DWD:$ECCODES_DEFINITIONS_BASE

# Generating process
MODEL_ASSIM_GP=61
MODEL_ASSIM_INC_GP=61
MODEL_FCAST_GP=62
MODEL_FCRUC_GP=64
MODEL_FCENS_GP=65
MODEL_FCI2I_GP=66
MODEL_INTER_GP=63
MODEL_ARKI_TGP_ASSIM=0
MODEL_ARKI_TGP_ASSIM_INC=201
MODEL_ARKI_TIMERANGE_ASSIM="origin:GRIB2,,,0,,$MODEL_ASSIM_GP"
MODEL_ARKI_TIMERANGE_ASSIM_INC="origin:GRIB2,,,201,,$MODEL_ASSIM_INC_GP"
MODEL_ARKI_TIMERANGE_FCAST="origin:GRIB2,,,2,,$MODEL_FCAST"
ANA_EXT=0000
ANA_DET_EXT=.det
IMPORT_THREAD=output

# postprocessing
CROSS_COORD_FILE=$WORKDIR_BASE/nwprun/conf/cross.shp
# start output at beginning by default
OUTPUT_START=0
