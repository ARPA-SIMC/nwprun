# Model environment variables
MODEL_NAME=hres
MODEL_DATADIR=$WORKDIR/model
MODEL_ARKI_DS=$ARKI_DIR/hres_am_foricon
MODEL_STOP=0
FC_LENGTH=72
ENS_TOTAL_MEMB=0
INI_TIMES=("00" "12")

# Da tenere per il check_run, da ripensare dopo
MODEL_DELTABD=0
MODEL_BACK=0

# Environment variable definition for MEC and LETKF
MEC_WORKDIR=$WORKDIR/mec

# MEC verification (modified version of executables)
DACE_BASE=/g100_work/smr_prod/srcintel/dace_code_2.20_ifs
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
LETKF_CONST=$DACE_BASE/data

# Archiviazione locale files di verifica
ARC_LOC=$WORKDIR/archive

# Stop suite in case of errors
STOP_ON_FAIL=Y

## suite timing
#NWPWAITELAPS=14400
## differenza tra tempo nominale e tempo di attivazione della suite
#NWPWAITSOLAR_RUN=1800
## dopo quando tempo rinuncio a girare la suite e passare alla successiva
#NWPWAITSOLAR=14400
#NWPWAITWAIT=60
## wait for analysis?
#WAIT_ANALYSIS=Y
