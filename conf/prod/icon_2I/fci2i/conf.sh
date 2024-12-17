# Parent model environment variables
PARENTMODEL=ICON
PARENTMODEL_ARKI_DS=$ARKI_DIR/icon_eu_dwd_foricon
PARENTMODEL_SIGNAL=iconeu_dwd_foricon
PARENTMODEL_FREQINI=12
PARENTMODEL_FREQANA=12
PARENTMODEL_FREQFC=1
PARENTMODEL_STATIC=$HOME/data/icon
PARENTMODEL_STATICFILE=iconeu_dwd_foricon_extpar_R03B08_N02_20231113_tiles.nc
PARENTMODEL_GRIDFILE=iconeu_dwd_foricon_grid_R03B08_N02_20231113_tiles.nc

# Model environment variables
MODEL_BASE=$HOME/srcgnu/icon
MODEL_BIN=$MODEL_BASE/bin/icon
ECRAD_DATA=$MODEL_BASE/data
MODEL_STATIC=$HOME/data/icon
MODEL_PRE_BINDIR=$HOME/srcgnu/dwd_icon_tools/icontools

# Dataset arkimet for observations
ARKI_LHN_DS=$ARKI_DIR/icon_2I_radar
MODEL_LHN_DT=600

# ICON-2I domain and grid files
DOMAIN=ICON_REG3_DOM01	
LOCALGRID=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.nc
LOCALGRID_PARENT=$MODEL_STATIC/domain_$DOMAIN/${DOMAIN}.parent.nc
LOCALGRID_EXTERNAL=$MODEL_STATIC/domain_$DOMAIN/external_parameter_icon_${DOMAIN}_tiles.nc

# Model environment variables
MODEL_BACK=0
MODEL_STOP=48 #72
MODEL_BCANA=N
MODEL_FREQINI=12
ENS_TOTAL_MEMB=0

# Time difference between model and parent reftime 
MODEL_DELTABD=0

# Number of boundary conditions handled by each task
NBC_PER_TASK=1

# Latent Heat Nudging (LHN)
MODEL_LHN=.TRUE.
MODEL_NH_LHN=4

# setup for arkilocal
ARKI_DIR=$WORKDIR/arki
# setup for remote import
unset ARKI_IMPDIR
#ARKI_SYNCDIR=$WORKDIR_BASE/import/sync.ftpmodelin
unset ARKI_DLDIR
CROSS_NETWORK=icon_2I_fci2i_c
VPROF_NETWORK=icon_2I_fci2i_v
MODEL_SIGNAL=icon_2I_fci2i

# suite timing
NWPWAITELAPS=14400
# differenza tra tempo nominale e tempo di attivazione della suite
NWPWAITSOLAR_RUN=5400
# dopo quando tempo rinuncio a girare la suite e passare alla successiva
NWPWAITSOLAR=36000
NWPWAITWAIT=60
# wait for analysis?
WAIT_ANALYSIS=N
# to be removed
STOP_ON_FAIL=Y
