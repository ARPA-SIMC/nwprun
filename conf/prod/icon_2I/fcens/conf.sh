# Parent model environment variables
PARENTMODEL=IFS
PARENTMODEL_ARKI_DS=$ARKI_DIR/ifsens_am_foricon
PARENTMODEL_SIGNAL=(ifsens_am_enda ifsens_am_endabak ifsens_am_eps)
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=1
GET_ICBC_MINCOUNT=940 # exactly 948

# input data
PARENTMODEL_DATADIR=$WORKDIR/input.$ENS_MEMB/data
MODEL_ARKI_PARAM="proddef:GRIB:pf=$ENS_MEMB or GRIB:nn=$ENS_MEMB;origin:GRIB2,98 or GRIB1,98"

# preprocessing (interpolation)
MODEL_PRE_WORKDIR=$WORKDIR/preicon.$ENS_MEMB
MODEL_PRE_DATADIR=$WORKDIR/preicon.$ENS_MEMB/data

# model run
MODEL_WORKDIR=$WORKDIR/icon.$ENS_MEMB
MODEL_DATADIR=$WORKDIR/icon.$ENS_MEMB/data

# setup for arkilocal
ARKI_DIR=$WORKDIR/arki.$ENS_MEMB    # forse inutile

# eps postprocessing
FXTR_HOME=$WORKDIR_BASE/srcgnu/Fieldextra_14.0.0
FXTR_BIN=$FXTR_HOME/src/fieldextra_gnu_opt_omp
FXTR_WORKDIR=$WORKDIR/fxtr
FXTR_DATADIR=$FXTR_WORKDIR/data
POSTPROC_EPS_WORKDIR=$WORKDIR/postproc_eps
POSTPROC_EPS_DATADIR=$POSTPROC_EPS_WORKDIR/data
# data for infomet upload
INFOMET_UP_URL=ftp://ftpsimc.arpae.it/
INFOMET_UP_BASEDIR=PREVISIONI/AREA_LIMITATA/ICON_ENS
REGLIST="abru basi cala camp emro friu lazi ligu lomb marc moli piem pugl sard sici tosc tren umbr vdao vene"


# Model environment variables
MODEL_BACK=0
MODEL_STOP=51
MODEL_BCANA=N
MODEL_FREQINI=24
ENS_TOTAL_MEMB=20
MODEL_ARCHIVE_ANA=$WORKDIR/../enda/archive

# Time difference between model and parent reftime 
case $TIME in
    03 | 09 | 15 | 21)
        MODEL_DELTABD=9
        ;;
    00 | 06 | 12 | 18)
        MODEL_DELTABD=6
esac

# Number of boundary conditions handled by each task
NBC_PER_TASK=1

# Latent Heat Nudging (LHN)
MODEL_LHN=.TRUE.
MODEL_NH_LHN=4

# MEC verification (modified version of executables)
DACE_BASE=/g100_work/smr_prod/srcintel_thomas/dace_code_2.06_mec
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
MEC_WORKDIR=$WORKDIR/mec
LETKF_CONST=$DACE_BASE/data

# setup for arkilocal
ARKI_DIR=$WORKDIR/arki
# setup for remote import
unset ARKI_IMPDIR
if [ -n "$CINECA_MAIN_SYSTEM" ]; then
    ARKI_SYNCDIR=$WORKDIR_BASE/import/sync.lami
fi
ARKI_DLDIR=$WORKDIR_BASE/download
#POSTPROC_LIST=(lami_make_vprof)
#CROSS_NETWORK=icon_2I_fcruc_c
#VPROF_NETWORK=icon_2I_fcruc_v
MODEL_SIGNAL=icon_2I_fcens

# suite timing
NWPWAITELAPS=14400
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=1800
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=14400
NWPWAITWAIT=60
# wait for analysis?
WAIT_ANALYSIS=Y
# remove operationally!
STOP_ON_FAIL=N
