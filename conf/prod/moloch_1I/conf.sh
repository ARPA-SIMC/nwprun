# definitions for parent model
PARENTMODEL=COSMO
#PARENTMODEL_DATADIR=$WORKDIR/input/data
PARENTMODEL_FREQINI=12
PARENTMODEL_FREQANA=1
PARENTMODEL_FREQFC=1
#PARENTMODEL_QRQS=.TRUE.
#PARENTMODEL_ARKI_DS=http://maialinux.metarpa:8090/dataset/cosmo_5I
PARENTMODEL_SIGNAL=cosmo_5I_fcast


# preprocessing (interpolation)
MODEL_PRE_WORKDIR=$WORKDIR/premoloch
MODEL_PRE_DATADIR=$WORKDIR/premoloch/data
MODEL_PRE_PRE_BIN=$HOME/srcgnu/NWP_ISAC_PACKAGE/moloch/executable_premodel/cosmo_to_premoloch
MODEL_PRE_BIN=$HOME/srcgnu/NWP_ISAC_PACKAGE/moloch/executable_premodel/premoloch
MODEL_POST_BIN=$HOME/srcgnu/NWP_ISAC_PACKAGE/common/executable_convert_shf_to_grib2/convert_shf_to_grib2
# model run
MODEL_WORKDIR=$WORKDIR/moloch
MODEL_DATADIR=$WORKDIR/moloch/data
MODEL_BIN=$HOME/srcgnu/NWP_ISAC_PACKAGE/moloch/executable_model/moloch
MODEL_STATIC=$ARCHIVE/geo_dataset

# configuration of assimilation
#MODEL_BCANA=Y
# difference (hours) between $DATE$TIME (end of assimilation window /
# start of forecast) and start of last available input forecast
# providing BC (for MODEL_BCANA=N)
MODEL_DELTABD=0
MODEL_BACK=0
MODEL_STOP=48
MODEL_BCANA=N
MODEL_ARCHIVE_ANA=$WORKDIR/../../cosmo_5I/fcast/archive

NWPWAITELAPS=10800
NWPWAITSOLAR=36000
NWPWAITSOLAR_RUN=7200
NWPWAITWAIT=30

# reconfigure arkimet for remote archiving at Cineca
ARKI_SCAN_METHOD=remote_arki_importer
ARKI_IMPDIR=~arki-imp/arki-imp/generic

