# Working directories for observations
BUFR_WORKDIR=$WORKDIR/bufr

# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_DIR/mars_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_DIR/mars_bufr_noconv
BUFR_ARKI_DS_SIGNAL=mars_bufr
FREQ_FILE_BUFR=1 #improve

# eccodes definitions
# Leonardo: ecd_dwd=$WORKDIR_BASE/data/definitions/definitions.edzw-2.21.0-1
# Leonardo: ecd_cin=$WORKDIR_BASE/data/definitions/definitions.cineca-2.21.0
ecd_dwd=/ind2/meteo/a07smr03/icon/definitions/definitions.edzw-2.21.0-1
ecd_cin=/ind2/meteo/a07smr03/icon/definitions/definitions.cineca-2.21.0
ECCODES_DEFINITION_PATH=${ecd_dwd}:${ecd_cin}
unset ecd_dwd ecd_cin

# Constant fields
MODEL_STATIC=$WORKDIR_BASE/data/ifs


## Generating process
#MODEL_ASSIM_GP=61
#MODEL_ASSIM_INC_GP=61
#MODEL_FCAST_GP=62
#MODEL_FCRUC_GP=64
#MODEL_FCENS_GP=65
#MODEL_INTER_GP=63
#MODEL_ARKI_TGP_ASSIM=0
#MODEL_ARKI_TGP_ASSIM_INC=201
#MODEL_ARKI_TIMERANGE_ASSIM="origin:GRIB2,,,0,,$MODEL_ASSIM_GP"
#MODEL_ARKI_TIMERANGE_ASSIM_INC="origin:GRIB2,,,201,,$MODEL_ASSIM_INC_GP"
#MODEL_ARKI_TIMERANGE_FCAST="origin:GRIB2,,,2,,$MODEL_FCAST"
#ANA_EXT=0000
#ANA_DET_EXT=.det
