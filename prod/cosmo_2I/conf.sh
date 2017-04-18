# RADAR for LHN
ARKI_LHN_DS=$ARKI_URL/cosmo_2I_radar
#RADAR_LHN_GP=10
MODEL_LHN=.FALSE.
MODEL_LHN_WORKDIR=$WORKDIR/lhn
MODEL_LHN_DT=10

# observations for nudging
BUFR_WORKDIR=$WORKDIR/bufr
MODEL_NUDG=.TRUE.

# _GP=10(radarobs),11(an),12(fc)
# definitions for parent model
PARENTMODEL=IFS
PARENTMODEL_DATADIR=$WORKDIR/input/data
PARENTMODEL_ARKI_DS=$ARKI_URL/ifs_med
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=3
PARENTMODEL_QRQS=.FALSE.

# definitions for the model run
MODEL_NLEV=65
# preprocessing (interpolation)
MODEL_PRE_WORKDIR=$WORKDIR/int2lm
MODEL_PRE_DATADIR=$WORKDIR/int2lm/data
#MODEL_PRE_BIN=$WORK/srcgnu/int2lm_150611_2.02/tstint2lm
MODEL_PRE_BIN=/gpfs/meteo/lami/srcgnu/int2lm_150611_2.02/tstint2lm
# model run
MODEL_WORKDIR=$WORKDIR/cosmo
MODEL_DATADIR=$WORKDIR/cosmo/data
#MODEL_BIN=$WORK/srcgnu/cosmo_160510_5.04a/lmparbin_all
#MODEL_BIN=$WORK/srcgnu/cosmo_160712_5.04b_sp/lmparbin_all
#MODEL_BIN=/gpfs/meteo/lami/srcgnu/cosmo_161215_5.04e_beta/lmparbin_all
MODEL_BIN=$WORK/srcintel/cosmo_170123_5.04d3/lmparbin_all
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
# providing BC (for MODEL_BCANA=N)
# in reassim this is overridden in conf.sh because of MODEL_BCANA
# 00->12 12->00 => 12
# 06->00 18->12 => 6
if [ "$TIME" = "00" -o "$TIME" = "12" ]; then
    MODEL_DELTABD=12
elif [ "$TIME" = "09" -o "$TIME" = "21" ]; then
    MODEL_DELTABD=9
elif [ "$TIME" = "06" -o "$TIME" = "18" ]; then
    MODEL_DELTABD=6
else # 03 15
    MODEL_DELTABD=3
fi

# for distinguishing assimilation from forecast results
MODEL_ARKI_TIMERANGE_ASSIM="timerange:Timedef,0,254;proddef:GRIB:tod=0"
MODEL_ARKI_TIMERANGE_FCAST="timerange:Timedef,0,254;proddef:GRIB:tod=1"
ARKI_DS_ASSIM=$ARKI_URL/cosmo_2I_assim
ARKI_DS_FCAST=$ARKI_URL/cosmo_2I_fcast
ARKI_DS_INTER=$ARKI_URL/cosmo_2I_inter
MODEL_ASSIM_GP=11
MODEL_FCAST_GP=12
MODEL_FCRUC_GP=14
MODEL_INTER_GP=13
