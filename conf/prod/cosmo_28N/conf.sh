# RADAR for LHN
ARKI_LHN_DS=$ARKI_URL/cosmo_28N_radar
MODEL_LHN_WORKDIR=$WORKDIR/lhn
MODEL_LHN_DT=10

# observations for nudging
BUFR_WORKDIR=$WORKDIR/bufr

# definitions for parent model
PARENTMODEL_DATADIR=$WORKDIR/input/data

# definitions for the model run
MODEL_NLEV=45
# preprocessing (interpolation)
MODEL_PRE_WORKDIR=$WORKDIR/int2lm
MODEL_PRE_DATADIR=$WORKDIR/int2lm/data
MODEL_PRE_BIN=$HOME/srcgnu/int2lm_180226_2.05/tstint2lm
# model run
MODEL_WORKDIR=$WORKDIR/cosmo
MODEL_DATADIR=$WORKDIR/cosmo/data
MODEL_BIN=$HOME/srcgnu/cosmo_180802_5.05_1_dp/lmparbin_all
MODEL_STATIC=$HOME/data/cosmo

# configuration of assimilation
#MODEL_BCANA=Y
MODEL_SOIL_PARENT=N
MODEL_SNOW_PARENT=N
MODEL_LAKE=Y
MODEL_SLOW_PAST_H=144
MODEL_FREQINI=24

# for distinguishing assimilation from forecast results
MODEL_ARKI_TIMERANGE_ASSIM="timerange:Timedef,0,254;proddef:GRIB:tod=0"
MODEL_ARKI_TIMERANGE_FCAST="timerange:Timedef,0,254;proddef:GRIB:tod=1"
# setup for arkilocal
ARKI_DIR=$WORKDIR/arki
MODEL_ASSIM_GP=41
MODEL_FCAST_GP=42
MODEL_FCRUC_GP=44
MODEL_INTER_GP=43

#NWPWAITELAPS=10200
#NWPWAITSOLAR=36000
#NWPWAITWAIT=30

