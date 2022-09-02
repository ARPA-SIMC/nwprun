MODEL_BIN=/ind2/meteo/a07smr03/lami/srcintel/cosmo_5.08_radial_wind_noclut/radarbin_nud
PARENTMODEL=COSMO
MODEL_NUDG=.FALSE.
MODEL_LHN=.TRUE.
MODEL_NH_NUDG=3
MODEL_NH_LHN=3
MODEL_BACK=1
MODEL_STOP=1
MODEL_BCANA=N
MODEL_FREQINI=3
ENS_TOTAL_MEMB=36
ENS_DET_MEMB=Y

# warning ARKI_DIR is redefined later for other purposes, find a better way!
BUFR_ARKI_DS_CONV=$ARKI_DIR/gts_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_DIR/gts_bufr_noconv
BUFR_ARKI_DS_RADARVOL=$ARKI_DIR/radar_vol
FREQ_FILE_BUFR=1 #improve

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
    01 | 04 | 07 | 10 | 13 | 16 | 19 | 22)
        MODEL_DELTABD=4
        ;;
    02 | 05 | 08 | 11 | 14 | 17 | 20 | 23)
        MODEL_DELTABD=5
        ;;
    esac
    PARENTMODEL_ARKI_DS=$ARKI_DIR/cosmo_am_enda
    PARENTMODEL_SIGNAL=cosmo_am_enda
    PARENTMODEL_FREQINI=3
    PARENTMODEL_FREQANA=3
    PARENTMODEL_FREQFC=3
    PARENTMODEL_QRQS=.FALSE.
# input data
    PARENTMODEL_DATADIR=$WORKDIR/input.$ENS_MEMB/data
    MODEL_ARKI_PARAM="proddef:GRIB:pf=$ENS_MEMB;origin:GRIB2,80"
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
    01 | 13)
        MODEL_DELTABD=1
        ;;
    02 | 14)
        MODEL_DELTABD=2
        ;;
    03 | 15)
        MODEL_DELTABD=3
        ;;
    04 | 16)
        MODEL_DELTABD=4
        ;;
    05 | 17)
        MODEL_DELTABD=5
        ;;
    06 | 18)
        MODEL_DELTABD=6
        ;;
    07 | 19)
        MODEL_DELTABD=7
        ;;
    08 | 20)
        MODEL_DELTABD=8
        ;;
    09 | 21)
        MODEL_DELTABD=9
        ;;
    10 | 22)
        MODEL_DELTABD=10
        ;;
    11 | 23)
        MODEL_DELTABD=11
        ;;
    esac
# setup for arkilocal
    ARKI_DIR=$WORKDIR/arki
# setup for remote import and download
    ARKI_SCAN_METHOD=configured_importer
    unset ARKI_IMPDIR
    ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_lami
    ARKI_DLDIR=$WORKDIR_BASE/download
    POSTPROC_LIST=(lami_make_nit)
    MODEL_SIGNAL=cosmo_2I_assim
fi
MODEL_ARCHIVE_OUTPUT_ANA=$WORKDIR/archive
# disabled, no second cluster
#MODEL_ARCHIVE_OUTPUT_ANA_REMOTE=$CINECA_ARCHIVE_REMOTE:$CINECA_ARCHIVE_REMOTE_ANA
# letkf analysis
LETKF_WORKDIR=$WORKDIR/letkf
LETKF_DATADIR=$WORKDIR/letkf/data
LETKF_BIN=$WORKDIR_BASE/srcintel/dace_code_2.06/build/LINUX64.intel-mpi/bin/var3d

# Radar volumes assimilation
ACT_EMVORADO=.TRUE.
HDF5_WORKDIR=$WORKDIR/radar_vol
RADLIST="16101 16102 16103 16105 16106 16107 16112 16144 16199 16998 16999"

# suite timing
MODEL_WALLTIME=00:45:00
NWPWAITELAPS=14400
NWPWAITSOLAR=36000
NWPWAITSOLAR_RUN=1200
NWPWAITWAIT=30
