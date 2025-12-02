## Model environment variables
MODEL_NAME=icon_2I
MODEL_ARKI_DS=$ARKI_URL/icon_2I_unstr
#MODEL_DATADIR=$WORKDIR/model
MODEL_DATADIR=$WORKDIR/icon/data
MODEL_STOP=72
ENS_TOTAL_MEMB=0

# Da tenere per il check_run, da ripensare dopo
MODEL_DELTABD=0
MODEL_BACK=0

# Dataset arkimet for observations
BUFR_ARKI_DS_CONV=$ARKI_URL/mars_bufr_conv
BUFR_ARKI_DS_NOCONV=$ARKI_URL/mars_bufr_noconv
BUFR_ARKI_DS_SIGNAL=mars_bufr

# Environment variable definition for MEC and LETKF
MEC_WORKDIR=$WORKDIR/mec

# MEC verification (modified version of executables)
DACE_BASE=$WORKDIR_BASE/srcintel/dace_code_2.25_verif
MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
LETKF_CONST=$DACE_BASE/data

## MEC and LETKF executables
#DACE_BASE=/g100_work/smr_prod/srcintel/dace_code_2.15
#MEC_BIN=$DACE_BASE/build/LINUX64.intel-mpi/bin/mec
#LETKF_CONST=$DACE_BASE/data


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
