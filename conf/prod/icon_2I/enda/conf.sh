# Icon bin with EMVORADO
MODEL_BASE=/g100_work/smr_prod/srcintel_virginia/icon-2.6.5.1
MODEL_BIN=$MODEL_BASE/bin/icon
ECRAD_DATA=$MODEL_BASE/data

# Forecast length 
MODEL_BACK=1
MODEL_STOP=1
MODEL_BCANA=N
# output only at the end for analysis run
OUTPUT_START=$MODEL_STOP

# Number of ensemble members
ENS_TOTAL_MEMB=40

# Radar assimilation
ACT_EMVORADO=.TRUE.
HDF5_WORKDIR=$WORKDIR/radar_vol
HDF5_QUARANTINE=$WORKDIR/radar_vol_quarantine
RADLIST="16101 16102 16103 16105 16106 16107 16112 16144 16199 16998 16999"
MODEL_LHN=.TRUE.
MODEL_NH_LHN=3

# Time difference between model and parent reftime 
case $TIME in
    01 | 07 | 13 | 19)
        MODEL_DELTABD=7 
        ;;
    02 | 08 | 14 | 20)
        MODEL_DELTABD=8 
        ;;
    03 | 09 | 15 | 21)
        MODEL_DELTABD=9 
        ;;
    04 | 10 | 16 | 22)
        MODEL_DELTABD=10 
        ;;
    05 | 11 | 17 | 23)
        MODEL_DELTABD=11
        ;;
    00 | 06 | 12 | 18)
        MODEL_DELTABD=12
        ;;
esac

# Environment variable definition for model and parent
PARENTMODEL=IFS
if [ -n "$ENS_MEMB" ]; then
    PARENTMODEL_ARKI_DS=$ARKI_DIR/ifsens_am_foricon
    PARENTMODEL_SIGNAL=ifsens_am_enda
    PARENTMODEL_FREQINI=6
    PARENTMODEL_FREQANA=6
    PARENTMODEL_FREQFC=1
    GET_ICBC_MINCOUNT=940 # exactly 945

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
    ARKI_DIR=$WORKDIR/arki.$ENS_MEMB

else # deterministic run or analysis
    PARENTMODEL_ARKI_DS=$ARKI_DIR/hres_am_foricon
    PARENTMODEL_SIGNAL=hres_am_foricon
    PARENTMODEL_FREQINI=6
    PARENTMODEL_FREQANA=6
    PARENTMODEL_FREQFC=1
    GET_ICBC_MINCOUNT=1035 # exactly 1040

    # setup for arkilocal
    ARKI_DIR=$WORKDIR/arki

    # setup for remote import and download
    ARKI_SCAN_METHOD=configured_importer
    unset ARKI_IMPDIR
    ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_lami
    ARKI_DLDIR=$WORKDIR_BASE/download
    #POSTPROC_LIST=(lami_make_nit)
    MODEL_SIGNAL=icon_2I_assim
fi
MODEL_ARCHIVE_OUTPUT_ANA=$WORKDIR/archive

# Number of boundary conditions handled by each task
NBC_PER_TASK=1

# Environment variable definition for MEC and LETKF
MEC_WORKDIR=$WORKDIR/mec
LETKF_WORKDIR=$WORKDIR/letkf
LETKF_DATADIR=$WORKDIR/letkf/data

# MEC and LETKF executables
#DACE_BASE=/ind2/meteo/a07smr03/lami/srcintel/dace_code_2.06
DACE_BASE=/g100_work/smr_prod/srcintel/dace_code_2.15
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
LETKF_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/var3d
LETKF_CONST=$DACE_BASE/data

# suite timing
NWPWAITELAPS=10800
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=1200
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
#NWPWAITSOLAR=43200
NWPWAITSOLAR=32400
NWPWAITWAIT=30
# wait for analysis?
WAIT_ANALYSIS=N

