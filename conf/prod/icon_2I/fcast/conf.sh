PARENTMODEL=IFS
PARENTMODEL_ARKI_DS=$ARKI_DIR/hres_am_foricon
PARENTMODEL_SIGNAL=hres_am_foricon
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=1
MODEL_LHN=.FALSE.
MODEL_NH_LHN=24
MODEL_BACK=0
MODEL_STOP=21
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
NBC_PER_TASK=1

# suite timing
NWPWAITELAPS=10800
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=3000
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=43200
NWPWAITWAIT=60

# New
# MEC modificato per la verifica
DACE_BASE=/g100_work/smr_prod/srcintel_thomas/dace_code_2.06_mec
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
MEC_WORKDIR=$WORKDIR/mec
LETKF_CONST=$DACE_BASE/data

# Time difference between model and parent reftime 
case $TIME in
    03 | 09 | 15 | 21)
        MODEL_DELTABD=3
        ;;
    00 | 06 | 12 | 18)
        MODEL_DELTABD=0
esac

# Initialize forecast using IAU and KENDA increments
MODEL_ARCHIVE_ANA=/g100_scratch/userexternal/tgastald/prod/icon_2I/enda/archive
