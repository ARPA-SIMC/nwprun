#!/bin/sh

singularity remote list
echo "In case of error you may need to get a new access token"
echo "from cloud.sylabs.io and execute singularity remote login"

set -e
set -x
for img in Singularity.*; do
	suff=${img#*.}
	singularity build --remote library://dcesari/default/smnd:$suff $img
done


