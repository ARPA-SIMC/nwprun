#!/bin/sh


log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}


import_one() {

    case $1 in
	./configured/*)
	    [ -f "$1" ] || return 1
	    upfile=${1##*/}
	    updir=${1%/*}
	    case $upfile in
		start.sh)
		    if [ ! -f "$updir/started.sh" ]; then
			# -R to create remote dir
			rsync -ptR --chmod=ug=rwX $1 $ARKI_IMPSSH
			touch $updir/started.sh
		    fi
		    return 1 # 1 = nothing done
		    ;;
		end.sh)
		    # if upload finished, check if the folder is empty and erase
		    # at this stage i am authorised to remove rubbish
		    rm -f $updir/.??* $updir/*.tmp
		    if ! ls $updir | grep -v '\.sh$'>/dev/null; then
			rsync -ptR --chmod=ug=rwX $1 $ARKI_IMPSSH
			rm -f $updir/*.sh
			rmdir $updir || true # better leaving rubbish than failing
			log "done importing folder $updir"
		    fi
		    return 1 # 1 = nothing done
		    ;;
		*.sh)
		    # ignore other .sh
		    return 1 # 1 = nothing done
		    ;;
		*)
		    # sync only after start
		    if [ -f "$updir/started.sh" ]; then
			log "start syncing configured $1"
			rsync -ptR --chmod=ug=rwX --remove-source-files $1 $ARKI_IMPSSH
			log "done syncing $1"
		    fi
		    return # 0 = something done or retry
		    ;;
	    esac
	    ;;
#	*)
#	    return 1
#	    ;;
    esac
    rm -f $1
}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
#. $NWPCONFBINDIR/arki_tools.sh
# end of setup

# move these to ~/.nwpconf !!!
ARKI_SCAN_METHOD=configured_arki_importer
ARKI_IMPSSH=arki-imp@lami.hpc.cineca.it:/arkimet/arki-imp/generic
#ARKI_IMPSSH=arki-imp@lami.hpc.cineca.it:~arki-imp/test
#ARKI_IMPSSH=arki-imp@131.175.199.95:/arkimet/arki-imp/generic
ARKI_IMPDIR=$WORKDIR_BASE/import_sync

nonunique_exit
set -x

# security check
[ -n "$ARKI_IMPDIR" ] || exit 1
cd $ARKI_IMPDIR

while true; do
    donenothing=Y
    # tried with find -regex '.*/[^.][^/].*((?!tmp).)*$' or
    # '.*/[^.][^/].*\(?!tmp\).$' unsuccessfully
    for file in `find . -type f -name '[^.]*'|grep -v '\.tmp$'`; do
        import_one $file && donenothing= || true
    done
    # if nothing has been done, exit
    if [ -n "$donenothing" ]; then
	exit 0
    fi
done
