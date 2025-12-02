if [ -n "$NWPCONF" ]; then
    export NWPCONFDIR=$WORKDIR_BASE/nwprun/conf
    export NWPCONFBINDIR=$WORKDIR_BASE/nwprun/libexec/nwpconf
# source the main nwpconf library module, other modules must be sourced
# in the job
    . $NWPCONFBINDIR/nwpconf.sh
fi
