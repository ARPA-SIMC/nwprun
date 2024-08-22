MODEL_NH_NUDG=4
MODEL_NH_LHN=4
MODEL_BACK=0
MODEL_STOP=72
MODEL_ARCHIVE_ANA=$WORKDIR/../assim/archive
MODEL_ARCHIVE_OUTPUT_EXTRA=$WORKDIR/archive
# LOGSIM_PROCESS is not defined here because the signal is sent by
# configured_multi_importer.sh when syncing
#LOGSIM_PROCESS=cosmo_5I_fcast
MODEL_SIGNAL=cosmo_5I_fcast
NWPWAITELAPS=10800
NWPWAITSOLAR=36000
NWPWAITSOLAR_RUN=5400
NWPWAITWAIT=30

# setup for remote import
unset ARKI_IMPDIR
ARKI_SYNCDIR=$WORKDIR_BASE/import/sync.ftpmodelin
unset ARKI_DLDIR
POSTPROC_LIST=(lami_make_arki5I)
#VPROF_NETWORK=cosmo_5I_fcast_vprof
