# RADAR for LHN
ARKI_LHN_DS=$ARKI_URL/COSMO_IT2_RADAR
#RADAR_LHN_GP=10
MODEL_LHN=.FALSE.
MODEL_LHN_WORKDIR=$WORKDIR/lhn

# observations for nudging
BUFR_WORKDIR=$WORKDIR/bufr
MODEL_NUDG=.TRUE.

# _GP=10(radarobs),11(an),12(fc)
# definitions for parent model
PARENTMODEL=IFS
PARENTMODEL_DATADIR=$WORKDIR/input/data
PARENTMODEL_ARKI_DS=$ARKI_URL/IFS_ME
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=3
PARENTMODEL_QRQS=.FALSE.

# definitions for the model run
MODEL_NLEV=65
# preprocessing (interpolation)
MODEL_PRE_WORKDIR=$WORKDIR/int2lm
MODEL_PRE_DATADIR=$WORKDIR/int2lm/data
MODEL_PRE_BIN=$WORK/srcgnu/int2lm_150611_2.02/tstint2lm
# model run
MODEL_WORKDIR=$WORKDIR/cosmo
MODEL_DATADIR=$WORKDIR/cosmo/data
MODEL_BIN=$WORK/srcgnu/cosmo_160510_5.04a/lmparbin_all
MODEL_STATIC=$WORK/data/cosmo

# configuration of assimilation
#MODEL_BCANA=Y
MODEL_SOIL_PARENT=N
MODEL_SNOW_PARENT=N
MODEL_LAKE=Y
MODEL_SLOW_PAST_H=144
MODEL_FREQINI=3
# difference (hours) between $DATE$TIME (end of assimilation window /
# start of forecast) and start of last available input forecast
# providing BC (for BCANA=N)
# in reassim this is overridden in conf.sh because of MODEL_BCANA
MODEL_DELTABD=0
#MODEL_DELTABD=6

# for distinguishing assimilation from forecast results
MODEL_ARKI_TIMERANGE_ASSIM="timerange:Timedef,0,254;proddef:GRIB:tod=0"
MODEL_ARKI_TIMERANGE_FCAST="timerange:Timedef,0,254;proddef:GRIB:tod=1"
ARKI_DS_ASSIM=$ARKI_URL/COSMO_IT2_ASSIM
ARKI_DS_FCAST=$ARKI_URL/COSMO_IT2_FCAST
ARKI_DS_INTER=$ARKI_URL/COSMO_IT2_INTER
MODEL_ASSIM_GP=11
MODEL_FCAST_GP=12
MODEL_INTER_GP=13
