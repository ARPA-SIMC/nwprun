PARENTMODEL=IFS
PARENTMODEL_ARKI_DS=$ARKI_DIR/hres_am_foricon
PARENTMODEL_SIGNAL=hres_am_foricon
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=1
PARENTMODEL_QRQS=.FALSE.
#MODEL_LHN=.TRUE.
#MODEL_NH_LHN=4
MODEL_BACK=0
MODEL_STOP=78
MODEL_BCANA=N
#MODEL_SOIL_PARENT=Y
#MODEL_SNOW_PARENT=Y
#MODEL_LAKE=Y
MODEL_FREQINI=6
# differenza tra reftime del modello e del padre
MODEL_DELTABD=0
ENS_TOTAL_MEMB=0
MODEL_SLOW_PAST_H=144
# setup for arkilocal
ARKI_DIR=$WORKDIR/arki
#MODEL_ARCHIVE_ANA=$WORKDIR/../enda/archive
# setup for remote import
ARKI_SCAN_METHOD=configured_importer
#unset ARKI_IMPDIR
#ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_lami
ARKI_DLDIR=$WORKDIR_BASE/download
#POSTPROC_LIST=(lami_make_arkiruc lami_make_vprof)
#VPROF_NETWORK=icon_2I_vprof
MODEL_SIGNAL=icon_2I_fcast
MODEL_ARKI_PARENT_STATIC="level:GRIB1,1 or GRIB2S,105,,1; product:GRIB1,,128,129 or GRIB2,98,0,3,4"
NBC_PER_TASK=1

# suite timing
NWPWAITELAPS=10800
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=3000
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=43200
NWPWAITWAIT=30
