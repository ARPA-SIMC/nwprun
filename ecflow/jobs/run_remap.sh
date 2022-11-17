#!/bin/bash 
#--------------------------------------------------------------------------------
# The script pre_ICON.sh uses the command "srun --ntasks" to simultaneously
# launch several instances of this script. 
# The number of the current instance is in the variable $SLURM_PROCID (range from
# 0 to ${SLURM_NTASKS}-1). Instance 0 is used for IC, the othe for BC
#--------------------------------------------------------------------------------

echo "Je suis $SLURM_PROCID de $SLURM_NTASKS avec $SLURM_CPUS_PER_TASK processeurs sur $SLURMD_NODENAME"

# needed in order to have shell functions in environment after srun
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/nwptime.sh
. $NWPCONFBINDIR/icon_model.sh
. $NWPCONFBINDIR/parcomp.sh
# end of setup

set -x

# Assigned variables
nh=$MODEL_STOP
ic=Y
fstep=$PARENTMODEL_FREQFC
nhsplit=$NBC_PER_TASK
IFS_ds=$(basename $PARENTMODEL_ARKI_DS)

# 0) creo la dir di lavoro specifica di questa istanza e ci vado dentro
mkdir -p ${MODEL_PRE_WORKDIR}/remap_${SLURM_PROCID}
cd ${MODEL_PRE_WORKDIR}/remap_${SLURM_PROCID}

# GRIB input to iconremap
export datafile=input.grib

# Initial conditions
if [ $SLURM_PROCID -eq 0 ] ; then
  if [ $ic = "Y" ] ; then
#    echo "Launch: remap_IC.sh "$project $ict_ver $compiler $rt $smi_flag $IFS_ds $cutoff" "$(date -u) 

	# NetCDF initial condition file name (output of iconremap)
	export out_file=$MODEL_PRE_DATADIR/ic_${DATES}${TIMES}.nc

	# field capacity/wilting point (fixed)
  	flc_file=${MODEL_STATIC}/${IFS_ds}/flc_${IFS_ds}.grb
  	wlt_file=${MODEL_STATIC}/${IFS_ds}/wlt_${IFS_ds}.grb
  	if [ ! -s $flc_file -o ! -s $wlt_file ] ; then
  	  echo "Input file(s) missing: $flc_file $wlt_file"
  	  exit 9
  	fi
  	rm -f swv.filt surface.grb soil.*.grb num.grb den.grb tmp.grb smi_geo.grb surf.g2

	# Put data on soil levels in separate files, 39-42 are the
	# grib1 parameters for soil water content, data in grib2 go
	# directly in the input file, topographies are removed
	cat << EOF > swv.filt
if (editionNumber == 1) {
  if (indicatorOfParameter == 39 || indicatorOfParameter == 40 || indicatorOfParameter == 41 || indicatorOfParameter == 42) {
    write "soil.[indicatorOfParameter:i].grb";
  } else {
    if (indicatorOfParameter == 129) {
      write "z_surface_dummy.grb";
    } else {
      write "surface.grb";
    }
  }
} else {
  if (discipline == 0 && parameterCategory == 3 && parameterNumber == 4) {
    write "z_hybrid_dummy.grb";
  } else {
    write "$datafile";
  }
}
EOF
  	grib_filter swv.filt $PARENTMODEL_DATADIR/$(inputmodel_name a)

	# Convert swv -> smi (soil moisture index)  
  	cdo sub $flc_file $wlt_file den.grb # denominator

  	for lev in 39 40 41 42; do
  	  echo "smi: process lev "$lev
  	  rm -f num.grb tmp.grb
	  cdo sub soil.${lev}.grb $wlt_file num.grb # numerator
  	  cdo div num.grb den.grb tmp.grb # num/den
  	  cat tmp.grb >> smi_geo.grb
  	done

	# convert all the grib1 messages to grib2 and append to
	# output, append also constant data (roughness is a temporary
	# patch)
  	grib_set -s editionNumber=2,scanningMode=0 surface.grb smi_geo.grb \
		 ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb \
		 ${MODEL_STATIC}/${IFS_ds}/sr_surface_${IFS_ds}.grb \
		 surf.g2

	# append the remaining data to the grib2 input file
  	cat surf.g2 \
	    ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb >> $datafile

        # if needed, here we should add fake W, (QR, QI), etc.
        # if QR QI not needed, remove from namelist
        grib_copy -w discipline=0,parameterCategory=1,parameterNumber=0 $datafile tmp.templ.g2
        grib_set -s parameterCategory=2,parameterNumber=8 -d 0. tmp.templ.g2 tmp.w.g2
        cat tmp.w.g2 >> $datafile

	# 2) Create namelist for iconremap
	conf_template iconremap_IC.nml	

	# 3) Run iconremap
	$MODEL_PRE_BINDIR/iconremap -vvv --remap_nml iconremap_IC.nml

  else
      echo "Do not process IC"
      exit 0
  fi

# Boundary conditions
else
  # this task must loop from h1 to h2 hours
  h1=$(( ($SLURM_PROCID-1)*$fstep*$nhsplit ))
  h2=$(( $h1+$fstep*($nhsplit-1) ))
  if [ $h2 -gt $nh ] ; then
    h2=$nh
  fi

  # names of intermediate files 
  export auxgrid="lateral_boundary" # grid file defining the lateral boundary
  # create iconsub namelist
  conf_template iconsub_BC.nml
  # run iconsub
  $MODEL_PRE_BINDIR/iconsub --nml iconsub_BC.nml

  # create iconremap namelist
  conf_template iconremap_BC.nml

  rm -f ncstorage.tmp*
  # Input grid information are taken from this file
  # export ecmgrid=$PARENTMODEL_DATADIR/$(inputmodel_name 0)
  # export ecmgrid=inputgrid.grib
  # grib_copy -w count=1 $PARENTMODEL_DATADIR/$(inputmodel_name 0) $ecmgrid

  # loop on hours to be processed
  for tt in $(seq $h1 $fstep $h2) ; do

#      cat $PARENTMODEL_DATADIR/$(inputmodel_name $tt) \
#	  ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb \
#	  ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb | \
#	  grib_set -s editionNumber=2 - $datafile

      # copy input converting all to grib2
#      grib_set -s editionNumber=2 \
#	       $PARENTMODEL_DATADIR/$(inputmodel_name $tt) $datafile

      # copy input removing topographies and converting all to grib2
	cat << EOF > topo_g2.filt
if (editionNumber == 1) {
  if (indicatorOfParameter == 129) {
    write "z_surface_dummy.grb";
  } else {
    set editionNumber = 2;
    write "$datafile";
  }
} else {
  if (discipline == 0 && parameterCategory == 3 && parameterNumber == 4) {
    write "z_hybrid_dummy.grb";
  } else {
    write "$datafile";
  }
}
EOF
  	grib_filter topo_g2.filt $PARENTMODEL_DATADIR/$(inputmodel_name $tt)
	# append topographies to boundary data converting to grib2
	grib_set -s editionNumber=2 \
		 ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb \
		 ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb topo.grib
	cat topo.grib >> $datafile
	rm -f topo.grib
      
      # append topographies to boundary data if not yet present (check
      # if roughness/lsm are needed)
#      nzh=$(grib_get -p editionNumber -w shortName=z,typeOfLevel=hybrid,level=1 $PARENTMODEL_DATADIR/$(inputmodel_name $tt)|wc -l)
#      if [ "$nzh" -eq 0 ]; then
#	  echo "appending zh"
#	  grib_set -s editionNumber=2 \
#		   ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb tmp1.grib
#		   cat tmp1.grib >> $datafile
#	  rm -f tmp1.grib
#      fi
#      nzs=$(grib_get -p editionNumber -w shortName=z,typeOfLevel=surface $PARENTMODEL_DATADIR/$(inputmodel_name $tt)|wc -l)
#      if [  "$nzs" -eq 0 ]; then
#	  echo "appending zs"
#	  grib_set -s editionNumber=2 \
#		   ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb tmp1.grib
#	  cat tmp1.grib >> $datafile
#	  rm -f tmp1.grib
#      fi
	  
      
      # if needed, here we should add fake W, (QR, QI), etc.
      # if QR QI not needed, remove from namelist
      grib_copy -w shortName=QV $datafile tmp.templ.g2
      #grib_copy -w discipline=0,parameterCategory=1,parameterNumber=0 $datafile tmp.templ.g2
      grib_set -s parameterCategory=2,parameterNumber=8 -d 0. tmp.templ.g2 tmp.w.g2
      cat tmp.w.g2 >> $datafile

      vtime=$(datetime_add $DATES $TIMES $tt)
      export out_file=$MODEL_PRE_DATADIR/lbc_${DATES}${TIMES}_${vtime}.nc
      rm -f $out_file

      # create iconremap main namelist
      conf_template iconremap_main_BC.nml

      # run iconremap
      $MODEL_PRE_BINDIR/iconremap -vvv --remap_nml iconremap_main_BC.nml \
			       --input_field_nml iconremap_BC.nml

      if [ ! -s $out_file ] ; then
	  echo "Error after iconremap in run_remap.sh"
	  exit 10
      fi
  done

fi
