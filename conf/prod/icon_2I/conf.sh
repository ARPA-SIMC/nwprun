MODEL_BASE=/ind2/meteo/a07smr03/icon/build/icon-2.6.4.intel.ECrad
MODEL_BIN=$MODEL_BASE/bin/icon
ECRAD_DATA=$MODEL_BASE/data
MODEL_STATIC=$WORKDIR_BASE/data/icon
MODEL_PRE_BINDIR=/ind2/meteo/a07smr03/icon/build/icontools-2.5.0.intel/icontools
PARENTMODEL_DATADIR=$WORKDIR/input/data
MODEL_PRE_WORKDIR=$WORKDIR/preicon
MODEL_PRE_DATADIR=$WORKDIR/preicon/data
MODEL_WORKDIR=$WORKDIR/icon
MODEL_DATADIR=$WORKDIR/icon/data

# ICON-I2 domain and grid files
DOMAIN=ICON_REG3_DOM01	
LOCALGRID=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.nc
LOCALGRID_PARENT=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.parent.nc
LOCALGRID_EXTERNAL=$MODEL_STATIC/domain_$DOMAIN/external_parameter_icon_${DOMAIN}_tiles.nc

# eccodes definitions
ecd_dwd=/ind2/meteo/a07smr03/icon/definitions/definitions.edzw-2.21.0-1
ecd_cin=/ind2/meteo/a07smr03/icon/definitions/definitions.cineca-2.21.0
ECCODES_DEFINITION_PATH=${ecd_dwd}:${ecd_cin}
unset ecd_dwd ecd_cin

# per sperimentare
STOP_ON_FAIL=Y
