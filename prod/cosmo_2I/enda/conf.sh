MODEL_NUDG=.FALSE.
MODEL_LHN=.TRUE.
MODEL_NH_NUDG=6
MODEL_NH_LHN=6
MODEL_BACK=6
MODEL_STOP=6
MODEL_BCANA=Y
MODEL_FREQINI=6
#LOGSIM_PROCESS=rucn2_ope_ra_run
# redefine directories for perturbed members
if [ -n "$ENS_MEMB" ]; then
# preprocessing (interpolation)
    MODEL_PRE_WORKDIR=$WORKDIR/int2lm.$ENS_MEMB
    MODEL_PRE_DATADIR=$WORKDIR/int2lm.$ENS_MEMB/data
# model run
    MODEL_WORKDIR=$WORKDIR/cosmo.$ENS_MEMB 
    MODEL_DATADIR=$WORKDIR/cosmo.$ENS_MEMB/data
# setup for arkilocal
    ARKI_URL=$WORKDIR/arki.$ENS_MEMB
else
# setup for arkilocal
    ARKI_URL=$WORKDIR/arki
fi
# letkf analysis
LETKF_WORKDIR=$WORKDIR/letkf
LETKF_DATADIR=$WORKDIR/letkf/data
LETKF_BIN=$WORK/srcgnu/3dvar/build/obj/var3d
