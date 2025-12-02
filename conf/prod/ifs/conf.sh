# Working directories for observations
BUFR_WORKDIR=$WORKDIR/bufr

# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_URL/mars_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_URL/mars_bufr_noconv
BUFR_ARKI_DS_SIGNAL=mars_bufr
FREQ_FILE_BUFR=1 #improve

# eccodes definitions
ECCODES_DEFINITION_PATH=$ECCODES_DEFINITIONS_DWD:$ECCODES_DEFINITIONS_BASE

# Constant fields
MODEL_STATIC=$WORKDIR_BASE/data/ifs
