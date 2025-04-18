#!/bin/sh

SYLABS_REPO=dcesari/default
SYLABS_PACKAGE=simctools

singularity remote list
echo "In case of error you may need to get a new access token"
echo "from cloud.sylabs.io by executing `singularity remote login`"

set -e
set -x
for img in Singularity.*_r8 Singularity.*_f40; do
	suff=${img#*.}
	singularity build --remote library://$SYLABS_REPO/$SYLABS_PACKAGE:$suff $img
done


