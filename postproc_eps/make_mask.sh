#!/bin/bash

TMPL=$1
OUTDIR=$2
for file in shapefiles/*.shp; do
    outname=${file##*/}
    outname=$OUTDIR/${outname%.*}.grib
    if [ ! -f "$outname" ]; then
	echo "Generating $outname from $file"
	vg6d_transform --trans-type=maskgen --sub-type=poly \
		       --coord-format=shp --coord-file=$file \
		       $TMPL $outname
    else
	echo "$outname exists, please remove it before proceeding"
    fi
done

	
    
