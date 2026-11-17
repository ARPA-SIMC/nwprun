# Model environment variables
MODEL_NAME=ifs-ens
MODEL_ARKI_DS=$ARKI_URL/ifsens_am_foricon
MODEL_DATADIR=$WORKDIR/ifs
MODEL_STATIC=/ind2/meteo/a07smr03/lami/data/ifs
MODEL_STOP=0
FC_LENGTH=60
ENS_TOTAL_MEMB=20
MODEL_FREQINI=12        # verosimilmente inutile
INI_TIMES=("00" "12")
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

# Archiviazione locale files di verifica
ARC_LOC=$WORKDIR/archive

# Stop suite in case of errors
STOP_ON_FAIL=Y

## suite timing
# suite timing
NWPWAITELAPS=14400
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=259200
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=259200
NWPWAITWAIT=60

