# Parent model environment variables
PARENTMODEL=IFS
PARENTMODEL_ARKI_DS=$ARKI_DIR/hres_am_foricon
PARENTMODEL_SIGNAL=hres_am_foricon
PARENTMODEL_FREQINI=6
PARENTMODEL_FREQANA=6
PARENTMODEL_FREQFC=1

# Model environment variables
MODEL_BACK=0
MODEL_STOP=6
MODEL_BCANA=N
MODEL_FREQINI=6
ENS_TOTAL_MEMB=0
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
ARKI_SCAN_METHOD=configured_importer
#unset ARKI_IMPDIR
ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_lami
ARKI_DLDIR=$WORKDIR_BASE/download
#POSTPROC_LIST=(lami_make_vprof)
CROSS_NETWORK=icon_2I_fcast_c
VPROF_NETWORK=icon_2I_fcast_v
MODEL_SIGNAL=icon_2I_fcast

# suite timing
NWPWAITELAPS=10800
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=3000
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=43200
NWPWAITWAIT=60
# wait for analysis?
WAIT_ANALYSIS=Y
# remove operationally!
STOP_ON_FAIL=Y
