#!/bin/bash 
#--------------------------------------------------------------------------------
# This job "preicon.ecf" uses the command "srun --ntasks" to simultaneously
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
. $NWPCONFBINDIR/getarki.sh
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arkilocal.sh
. $NWPCONFBINDIR/arki_tools.sh
# end of setup

set -x

# Define name of Arkimet dataset and GRIB input filename for iconremap
IFS_ds=$(basename $PARENTMODEL_ARKI_DS)
ic="Y"
export datafile=input.grib

# Create working directory for this task
mkdir -p ${MODEL_PRE_WORKDIR}/remap_${SLURM_PROCID}
cd ${MODEL_PRE_WORKDIR}/remap_${SLURM_PROCID}


# ----------------------------------------------------------------------------------
# Initial conditions
# ----------------------------------------------------------------------------------
if [ $SLURM_PROCID -eq 0 ] ; then
    # ------------------------------------------------------------------------------
    # Interpolate analysis from IFS
    # ------------------------------------------------------------------------------
    if [ $ic = "Y" ] ; then
        # NetCDF initial condition file name (output of iconremap)
        echo "No first guess and increments. Process analysis from parent model"
        export out_file=$MODEL_PRE_DATADIR/ic_${DATES}${TIMES}.nc

        # Retrieve field capacity/wilting point (fixed)
        flc_file=${MODEL_STATIC}/${IFS_ds}/flc_${IFS_ds}.grb
        wlt_file=${MODEL_STATIC}/${IFS_ds}/wlt_${IFS_ds}.grb
        if [ ! -s $flc_file -o ! -s $wlt_file ] ; then
          echo "Input file(s) missing: $flc_file $wlt_file"
          exit 9
        fi

        # Extract and discard (if present)  topography, land-sea mask, soil type, 
        # surface roughness length (GRIB 1, parameters 129, 173, 172, 43; GRIB 2, 
        # triplet 0,3,4), since they will be taken from MODEL_STATIC.
        # Extract and save, in separate files, soil water content (GRIB 1, parameters
        # 39-42), other surface fields (other GRIB 1 data) and atmospheric fields 
        # (other GRIB 2 data).
	    cat << EOF > swv.filt
        if (editionNumber == 1) {
          if (indicatorOfParameter == 39 || indicatorOfParameter == 40 || indicatorOfParameter == 41 || indicatorOfParameter == 42) {
            write "soil.[indicatorOfParameter:i].grb";
          } else {
            if (indicatorOfParameter == 129 || indicatorOfParameter == 173 || indicatorOfParameter == 172|| indicatorOfParameter == 43) {
              write "surface_dummy.grb";
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

        # Convert all the GRIB1 messages (saved surface fields and SMI) to GRIB2 and 
        # append to the same file "surf.g2". Append also constant data (topography,
        # (topography, land sea mask, soil type, surface roughness length) taken
        # from MODEL_STATIC.
  	    grib_set -s editionNumber=2,scanningMode=0 surface.grb smi_geo.grb  \
	    	 ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb              \
	    	 ${MODEL_STATIC}/${IFS_ds}/sr_surface_${IFS_ds}.grb             \
                 ${MODEL_STATIC}/${IFS_ds}/lsm_surface_${IFS_ds}.grb            \
                 ${MODEL_STATIC}/${IFS_ds}/slt_${IFS_ds}.grb                    \
	    	 surf.g2

        # If SST on land is 0°C (instead of "missing"), set to missing values on
        # (partly) land cells. Adapted from Enrico's patch. Necessary for 
        # experiments with non-corrected SST, may be removed later
        miss_sst=$(grib_get -w shortName=sst -P numberOfMissing surf.g2)
        if [ $miss_sst -eq 0 ] ; then
            # Apply filter to GRIB file
            echo "Mask SST land values"
            cat << EOF > sst.filt
            switch (shortName) {
              case 'sst' :
                write "sst_org.g2";
              case 'lsm' :
                write "lsm.g2";
              default :
                write "surf_others.g2";
            }
EOF
            grib_filter sst.filt surf.g2
            
            # Apply mask
            $SIMC_TOOLS vg6d_transform --trans-type=metamorphosis                 \
                --sub-type=maskvalid --coord-format=grib_api  --coord-file=lsm.g2 \
                --maskbounds=-1.,0. sst_org.g2 sst_msk.g2

            # Append masked SST to other surface fields
            cat lsm.g2 sst_msk.g2 surf_others.g2 > surf_new.g2
            mv surf_new.g2 surf.g2
        fi

	# Append the remaining GRIB2 data (surface data converted before and
        # z_hybrid from MODEL_STATIC) to atmospheric fields in "datafile"
  	    cat surf.g2 ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb >> $datafile

        # Check if vertical velocity is present. If not, append an empty field
        grib_copy -w parameterCategory=2,parameterNumber=8 $datafile w.grib
        if [ ! -f w.grib ]; then
            grib_copy -w discipline=0,parameterCategory=1,parameterNumber=0      \
                $datafile tmp.templ.g2
            grib_set -s parameterCategory=2,parameterNumber=8 -d 0. tmp.templ.g2 \
                tmp.w.g2
            cat tmp.w.g2 >> $datafile
        fi

	# Create namelist for iconremap
	conf_template iconremap_IC.nml	

	# Run iconremap
	$MODEL_PRE_BINDIR/iconremap -vvv --remap_nml iconremap_IC.nml


    # ------------------------------------------------------------------------------
    # Interpolate only SST analysis from IFS
    # ------------------------------------------------------------------------------
    else
        # GRIB file name for SST (output of iconremap)
        echo "Do not process IC, except for SST"
        export out_file_sst=$MODEL_PRE_DATADIR/ic_${DATES}${TIMES}_sst.grib

        # Extract SST
        grib_copy -w shortName='sst' $PARENTMODEL_DATADIR/$(inputmodel_name a) \
            sst.grib1

        # If SST has no missing values, check if file "sst_ifs_hres.grib" exists 
        # (this is SST from IFS-HRES for ensemble members to be employed at 00 UTC
        # for experiments). TO BE REOVED FOR OPERATIONAL IMPLEMENTATION.
        # In any case, convert to GRIB2.
        miss_sst=$(grib_get -P numberOfMissing sst.grib1)
        if ([ $miss_sst -eq 0 ] && [ -s $PARENTMODEL_DATADIR/sst_ifs_hres.grib ]) ; then
            grib_set -s editionNumber=2,scanningMode=0 \
                    $PARENTMODEL_DATADIR/sst_ifs_hres.grib sst.grib
        else
            grib_set -s editionNumber=2,scanningMode=0 sst.grib1 sst.grib 
        fi

        # Interpolate on ICON grid. This is done only if missing values are present
        # (this check can be REMOVED for operational implementation)
        miss_sst_new=$(grib_get -P numberOfMissing sst.grib)
        if [ $miss_sst_new -gt 0 ]; then
            conf_template iconremap_SST.nml
            $MODEL_PRE_BINDIR/iconremap -vvv --remap_nml iconremap_SST.nml
        fi
    fi


# ----------------------------------------------------------------------------------
# Boundary conditions
# ----------------------------------------------------------------------------------
else
    # This task must loop from h1 to h2 hours
    h1=$(( ($SLURM_PROCID-1)*$PARENTMODEL_FREQFC*$NBC_PER_TASK ))
    h2=$(( $h1+$PARENTMODEL_FREQFC*($NBC_PER_TASK-1) ))
    if [ $h2 -gt $MODEL_STOP ] ; then
      h2=$MODEL_STOP
    fi

    # Create namelist for iconsub and run it: this creates the auxiliary grid file
    # which contains only the cells of the boundary zone
    conf_template iconsub_BC.nml
    $MODEL_PRE_BINDIR/iconsub --nml iconsub_BC.nml

    # Create iconremap namelist and clean-up
    conf_template iconremap_BC.nml
    rm -f ncstorage.tmp*

    # Loop on hours to be processed
    for tt in $(seq $h1 $PARENTMODEL_FREQFC $h2) ; do
        # Copy input removing topographies and converting all to GRIB2
	    cat << EOF > topo_g2.filt
        if (editionNumber == 1) {
          if (indicatorOfParameter == 129 || indicatorOfParameter == 173) {
            write "surface_dummy.grb";
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

	    # Append topographies to boundary data converting to grib2
	    grib_set -s editionNumber=2 \
	    	 ${MODEL_STATIC}/${IFS_ds}/z_hybrid_${IFS_ds}.grb \
	    	 ${MODEL_STATIC}/${IFS_ds}/z_surface_${IFS_ds}.grb topo.grib
	    cat topo.grib >> $datafile
	    rm -f topo.grib
      
        # Check if vertical velocity is present. If not, append an empty field
        grib_copy -w parameterCategory=2,parameterNumber=8 $datafile w.grib
        if [ ! -f w.grib ]; then
            grib_copy -w discipline=0,parameterCategory=1,parameterNumber=0     \
                $datafile tmp.templ.g2
            grib_set -s parameterCategory=2,parameterNumber=8 -d 0. tmp.templ.g2 tmp.w.g2
            cat tmp.w.g2 >> $datafile
        fi

        # Define outpul boundary condition file name
        vtime=$(datetime_add $DATES $TIMES $tt)
        export out_file=$MODEL_PRE_DATADIR/lbc_${DATES}${TIMES}_${vtime}.nc
        rm -f $out_file

        # Create iconremap main namelist
        conf_template iconremap_main_BC.nml

        # Run iconremap and check if file is really created
        $MODEL_PRE_BINDIR/iconremap -vvv --remap_nml iconremap_main_BC.nml \
	      	       --input_field_nml iconremap_BC.nml

        if [ ! -s $out_file ] ; then
	    echo "Error after iconremap in run_remap.sh"
	    exit 10
        fi
    done

fi
