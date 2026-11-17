# Model environment variables
MODEL_NAME=icon_2I
MODEL_ARKI_DS=$ARKI_URL/icon_2I_fcens_unstr
MODEL_DATADIR=$WORKDIR/icon      #note : /data is remove on purpose, for EPS
MODEL_STATIC=/ind2/meteo/a07smr03/lami/data/icon
MODEL_STOP=0
FC_LENGTH=51
ENS_TOTAL_MEMB=20
MODEL_FREQINI=12        # verosimilmente inutile
INI_TIMES=("21")
MODEL_OUT_FREQ=1
VER_FREQ=1

# Necessary for check_run, to be improved
MODEL_DELTABD=0
MODEL_BACK=0

# MEC environment variables
DACE_BASE=/ind2/meteo/a07smr03/lami/srcintel/dace_code_2.25_verif
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
LETKF_CONST=$DACE_BASE/data
MEC_WORKDIR=$WORKDIR/mec

#Icon domain paths
DOMAIN=ICON_REG3_DOM01
LOCALGRID=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.nc
LOCALGRID_PARENT=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.parent.nc
LOCALGRID_EXTERNAL=$MODEL_STATIC/domain_$DOMAIN/external_parameter_icon_${DOMAIN}_tiles.nc




# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_URL/gts_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_URL/gts_bufr_noconv
BUFR_ARKI_DS_SIGNAL=gts_bufr


# Archiviazione locale files di verifica
ARC_LOC=$WORKDIR/archive

# Stop suite in case of errors
STOP_ON_FAIL=Y

# suite timing
NWPWAITELAPS=14400
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=259200
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=259200
NWPWAITWAIT=60

