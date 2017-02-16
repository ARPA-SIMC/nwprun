MODEL_NUDG=.FALSE.
MODEL_LHN=.TRUE.
MODEL_NH_NUDG=3
MODEL_NH_LHN=3
MODEL_BACK=0
MODEL_STOP=3
MODEL_BCANA=N
MODEL_FREQINI=3

PARENTMODEL=COSMO

# redefine directories for perturbed members
if [ -n "$ENS_MEMB" ]; then
    MODEL_DELTABD=3
    PARENTMODEL_ARKI_DS=$ARKI_URL/cnmc_cosmo_eps
    PARENTMODEL_FREQINI=3
    PARENTMODEL_FREQANA=3
    PARENTMODEL_FREQFC=3
    PARENTMODEL_QRQS=.FALSE.
# input data
    PARENTMODEL_DATADIR=$WORKDIR/input.$ENS_MEMB/data
    MODEL_ARKI_PARAM="proddef:GRIB:nn=$ENS_MEMB;"
# preprocessing (interpolation)
    MODEL_PRE_WORKDIR=$WORKDIR/int2lm.$ENS_MEMB
    MODEL_PRE_DATADIR=$WORKDIR/int2lm.$ENS_MEMB/data
# model run
    MODEL_WORKDIR=$WORKDIR/cosmo.$ENS_MEMB 
    MODEL_DATADIR=$WORKDIR/cosmo.$ENS_MEMB/data
# setup for arkilocal
    ARKI_URL=$WORKDIR/arki.$ENS_MEMB
else # deterministic run or analysis
#    MODEL_DELTABD=...
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
    PARENTMODEL_ARKI_DS=o_lm5_ope_forecast
    PARENTMODEL_FREQINI=12
    PARENTMODEL_FREQANA=1
    PARENTMODEL_FREQFC=1
    PARENTMODEL_QRQS=.TRUE.
# setup for arkilocal
    ARKI_URL=$WORKDIR/arki
fi
# letkf analysis
LETKF_WORKDIR=$WORKDIR/letkf
LETKF_DATADIR=$WORKDIR/letkf/data
LETKF_BIN=$WORK/srcgnu/3dvar/build/obj/var3d
