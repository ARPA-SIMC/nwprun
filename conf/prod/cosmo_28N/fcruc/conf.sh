# definitions for parent model
PARENTMODEL=COSMO
PARENTMODEL_FREQINI=12
PARENTMODEL_FREQANA=1
PARENTMODEL_FREQFC=1
PARENTMODEL_QRQS=.TRUE.
PARENTMODEL_ARKI_DS=http://maialtest.metarpa:8090/dataset/cosmo_5I

# configuration of assimilation
MODEL_SOIL_PARENT=Y
MODEL_SNOW_PARENT=N
MODEL_LAKE=Y
MODEL_SLOW_PAST_H=144
MODEL_FREQINI=3
MODEL_BACK=0
MODEL_STOP=18
MODEL_LHN=.TRUE.
MODEL_NH_LHN=4
MODEL_NUDG=.TRUE.
MODEL_NH_NUDG=4

# difference (hours) between $DATE$TIME (end of assimilation window /
# start of forecast) and start of last available input forecast
# providing BC (for MODEL_BCANA=N)
MODEL_BCANA=N
PARENTMODEL_SIGNAL=cosmo_5I_fcast
if [ "$TIME" = "00" -o "$TIME" = "12" ]; then
    MODEL_DELTABD=12
elif [ "$TIME" = "09" -o "$TIME" = "21" ]; then
    MODEL_DELTABD=9
elif [ "$TIME" = "06" -o "$TIME" = "18" ]; then
    MODEL_DELTABD=6
else # 03 15
    MODEL_DELTABD=3
fi

MODEL_ARCHIVE_OUTPUT_ANA=$WORKDIR/../assim/archive
#LOGSIM_PROCESS=
NWPWAITELAPS=7200
NWPWAITSOLAR=36000
NWPWAITSOLAR_RUN=3600
NWPWAITWAIT=30
