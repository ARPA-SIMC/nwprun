MODEL_BIN=$WORKDIR_BASE/srcintel/cosmo_180802_5.05_1_dp/lmparbin_all
PARENTMODEL=COSMO
MODEL_NUDG=.FALSE.
MODEL_LHN=.TRUE.
MODEL_NH_NUDG=3
MODEL_NH_LHN=3
MODEL_BACK=3
MODEL_STOP=3
MODEL_BCANA=N
MODEL_FREQINI=3
ENS_TOTAL_MEMB=36
ENS_DET_MEMB=Y

# redefine directories for perturbed members
if [ -n "$ENS_MEMB" ]; then
    case $TIME in
	00 | 12)
	    MODEL_DELTABD=6 # was 3
	    ;;
	03 | 15)
	    MODEL_DELTABD=6
	    ;;
	06 | 18)
	    MODEL_DELTABD=6 # was 3
	    ;;
	09 | 21)
	    MODEL_DELTABD=6
	    ;;
    esac
    PARENTMODEL_ARKI_DS=$ARKI_URL/cnmc_cosmo_eps
    PARENTMODEL_SIGNAL=${PARENTMODEL_ARKI_DS##*/}
    PARENTMODEL_SIGNAL_NFILES=3
    PARENTMODEL_FREQINI=3
    PARENTMODEL_FREQANA=3
    PARENTMODEL_FREQFC=3
    PARENTMODEL_QRQS=.FALSE.
# input data
    PARENTMODEL_DATADIR=$WORKDIR/input.$ENS_MEMB/data
    MODEL_ARKI_PARAM="proddef:GRIB:nn=$ENS_MEMB;origin:GRIB1,80"
# preprocessing (interpolation)
    MODEL_PRE_WORKDIR=$WORKDIR/int2lm.$ENS_MEMB
    MODEL_PRE_DATADIR=$WORKDIR/int2lm.$ENS_MEMB/data
# model run
    MODEL_WORKDIR=$WORKDIR/cosmo.$ENS_MEMB 
    MODEL_DATADIR=$WORKDIR/cosmo.$ENS_MEMB/data
# setup for arkilocal
    ARKI_DIR=$WORKDIR/arki.$ENS_MEMB
else # deterministic run or analysis
    PARENTMODEL_ARKI_DS=o_lm5_ope_forecast
    PARENTMODEL_FREQINI=12
    PARENTMODEL_FREQANA=1
    PARENTMODEL_FREQFC=1
    PARENTMODEL_QRQS=.TRUE.
    case $TIME in
	00 | 12)
	    MODEL_DELTABD=12
	    ;;
	03 | 15)
	    MODEL_DELTABD=3
	    ;;
	06 | 18)
	    MODEL_DELTABD=6
	    ;;
	09 | 21)
	    MODEL_DELTABD=9
	    ;;
    esac
# setup for arkilocal
    ARKI_DIR=$WORKDIR/arki
# setup for remote import
    ARKI_IMPDIR=~arki-imp/arki-imp/cosmo_2I_assim
fi
MODEL_ARCHIVE_OUTPUT_ANA=$WORKDIR/archive
MODEL_ARCHIVE_OUTPUT_ANA_REMOTE=login.meucci.cineca.it:/meucci_arpae/lami/prod/cosmo_2I/enda/archive
# letkf analysis
LETKF_WORKDIR=$WORKDIR/letkf
LETKF_DATADIR=$WORKDIR/letkf/data
LETKF_BIN=$WORKDIR_BASE/srcintel/dace_code/build/LINUX64.intel-mpi/bin/var3d

# scheduler resources
PARRES="-l select=2:ncpus=32:mpiprocs=32:mem=100gb -q meteoenda"
SERRES="-l select=1:ncpus=1:mpiprocs=1:mem=2gb -l place=pack:shared -q meteoenda"
MODEL_WALLTIME=00:45:00
DELAY=1
NWPWAITELAPS=14400
NWPWAITSOLAR=36000
NWPWAITSOLAR_RUN=3600
NWPWAITWAIT=30
OPERATIONS=OMA
