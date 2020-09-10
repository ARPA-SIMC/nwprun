#!/bin/bash

. ./cineca_get_common.sh

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/cosmo_2I/fcast_cineca_get

lami_cineca_get
