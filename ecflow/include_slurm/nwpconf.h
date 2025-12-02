# source desired  NWPCONF configuration
if [ -n "$NWPCONF" ]; then
    export NWPCONFDIR=$WORKDIR_BASE/nwprun/conf
    export NWPCONFBINDIR=$WORKDIR_BASE/nwprun/libexec/nwpconf
# source the main nwpconf library module, other modules must be sourced
# in the job
    . $NWPCONFBINDIR/nwpconf.sh
fi
# define exit_0 function if not defined
if [ ! $(type -t exit_0) ]; then
    exit_0() {
	exit 0
    }
fi
