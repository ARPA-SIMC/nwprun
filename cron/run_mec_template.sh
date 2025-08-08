#!/bin/ksh

#SBATCH --qos=qos_meteo
#SBATCH -A smr_prod
#SBATCH --time=00:20:00
#SBATCH --nodes=2
#SBATCH --partition=g100_meteo_prod
#SBATCH --ntasks-per-node=46
#SBATCH --sockets-per-node=2
#SBATCH --ntasks-per-socket=23
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=1
#SBATCH --mem=360G

# Load modules and export variables
. /etc/profile
module load autoload
module load intel-oneapi-compilers/2021.4.0
module load intel-oneapi-mpi/2021.4.0
module load intel-oneapi-mkl/2021.4.0
export LD_LIBRARY_PATH=$WORKDIR_BASE/srcintel/install/lib:$LD_LIBRARY_PATH

# Source script for parallel computing
. $OPE/libexec/nwpconf/parcomp.sh

#-----------------------------------------------------------------------------
# Ex. ./run_mec_template.sh 2020020306
#-----------------------------------------------------------------------------
# Notes:
# - PART III: RUN MEC Must be modified according to user specifications
# - It is assumed that model output is in the form of hourly files, containing
#   all the necessary fields
#-----------------------------------------------------------------------------
# Advance time, 10 digits date, ex. 2020020306 (observation date)
#-----------------------------------------------------------------------------
echo "\n advance time"
#-----------------------------------------------------------------------------
echo "Date must have 10 digits"
#-----------------------------------------------------------------------------
date3=$1
echo "Verify date " ${date3}
datein=${date3:0:8}
echo "First run " ${datein}
ant3=${date3:8:10}
echo "Lead time 1 = " ${ant3}
#-----------------------------------------------------------------------------
# Define some constants related to model run
#-----------------------------------------------------------------------------
#----------------------------
# number of forecast hours
nohfc=$2
#----------------------------
# eqv. of forecast hours in days
nodays=$(( ${nohfc} / 24 ))
echo $nohfc "hours forecast range ($nodays days)"
#----------------------------
# experiment name
EXP="icon2I"
echo "MEC run for " ${EXP}
#----------------------------
# model name
fispre="icon2I"   # THOMAS
echo "Model prefix " ${fispre}
#----------------------------
# feedback file name prefix
PROUT=ver 
echo "Feedback-file name ${PROUT}*"
#----------------------------
# switch for fg_file, required atmosphere fields
ranset=0
#-----------------------------------------------------------------------------
# Define pathways as much as possible
#-----------------------------------------------------------------------------
echo "Define pathways"
PATH_HOME=$WORKDIR_BASE/CARMA
PATH_WORK=${PATH_HOME}/OUTPUT
#-------------------------
# directory for observations, netcdf, cdfin_synop
OBS_DIR=${PATH_HOME}/COMMON_L
#-------------------------
# directory for constant files
PATH_CONST=${PATH_HOME}/INDATA/CONST
#-------------------------
# directory for forecast files
PATH_FC=${PATH_HOME}/ICONDATA
#-------------------------
# MEC executable
BIN=${PATH_HOME}/BIN/mec
#-------------------------
echo "JOB directory = " ${PATH_HOME}
echo "Main OUTPUT directory = " ${PATH_WORK}
echo "Observations directory = " ${OBS_DIR}
echo "Directory for constant files = " ${PATH_CONST}
echo "Directory for forecast files = " ${PATH_FC}
echo "MEC executable is " ${BIN}

#-----------------------------------------------------------------------------
# Set up Work directory
#-----------------------------------------------------------------------------
cd ${PATH_WORK}
WORKDIR=${PATH_WORK}/${date3:0:10}/${EXP}
echo "Work directory = " $WORKDIR
if [[ ! -d $WORKDIR ]] ; then
   echo
   echo  $WORKDIR is absent, create
   mkdir -p $WORKDIR
   mkdir -p $WORKDIR/const
   mkdir -p $WORKDIR/input
   mkdir -p $WORKDIR/output
   mkdir -p $WORKDIR/run
   mkdir -p $WORKDIR/tmp
else
   echo $WORKDIR already present, clean up
   rm -f $WORKDIR/output/*
   rm -f $WORKDIR/const/*
   rm -f $WORKDIR/run/*
   rm -f $WORKDIR/input/*
   rm -f $WORKDIR/tmp/*
fi
#-----------------------------------------------------------------------------
cd $WORKDIR/run
pwd
#-----------------------------------------------------------------------------
# PART I: Prepare data for MEC
#-----------------------------------------------------------------------------
echo "\n Prepare input data for MEC"
#-----------------------------------------------------------------------------
# Create links to required data, except forecasts
#-----------------------------------------------------------------------------
echo "create links"
#-----------------------------------------------------------------------------
# obs files, including blacklist file
#-----------------------------------------------------------------------------
# THOMAS: link each observation file:
for obsfile in `ls ${OBS_DIR}/${date3:0:10}/cdfin*`; do
    echo "Linking observation file = " ${obsfile}
    obsfilename=$(basename -- "$obsfile")
    obsfilename="${obsfilename%.*}"
    ln -s  ${obsfile} ${WORKDIR}/input/$obsfilename
done
echo "Linking blacklist file"
ln -s ${OBS_DIR}/blklsttmp ${WORKDIR}/input/
#-----------------------------------------------------------------------------
# constant grib files
# const folder and linking optional, eliminates long paths in namelist file
#-----------------------------------------------------------------------------
invar_det=${PATH_CONST}/ICON-2I_const.grib
echo "Linking Invar file = " ${invar_det}
ln -s ${invar_det} ${WORKDIR}/const/
const_grid=${PATH_CONST}/ww15mgh.grd
icon_grid=${PATH_CONST}/domain.nc
echo "Linking Mandatory grid file = " ${const_grid}
ln -s ${const_grid} ${WORKDIR}/const/
echo "Linking Mandatory grid file = " ${icon_grid}
ln -s ${icon_grid} ${WORKDIR}/const/

#-----------------------------------------------------------------------------
# Required field for reference atmosphere (can be one of the forecast files)
# linking and renaming optional, done to eliminate long paths in namelist file
#-----------------------------------------------------------------------------
FREFATM="${PATH_FC}/${fispre}_${datein}0000_+00${ant3}0000.grb"    
echo "Linking file for reference atmosphere = " ${FREFATM}
if [ -s ${FREFATM} ];then
   ln -s ${FREFATM} ${WORKDIR}/input/fg_file
   echo "Required field for reference atmosphere set"
   ranset=1
else
   echo "Required field for reference atmosphere NOT set"
fi
#-----------------------------------------------------------------------------
# Prepare forecast files
#-----------------------------------------------------------------------------
# FCS_LIST="0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72"
# replaced with computation of possible forecast steps
#-----------------------------------------------------------------------------
#-----------------------------------------
# Loop over forecast steps
#-----------------------------------------
for zi in $(seq -w 0 1 ${nodays}); do
   ant=$(( ${ant3#0} + (${zi} * 24) ))
   #-------------------------------------------------
   # Make sure forecast step is within forecast range
   #-------------------------------------------------
   if [ ${ant} -le ${nohfc} -a ${ant} -gt "0" ]; then
      echo "    "
      ant100=$(( $ant*100 ))
      if [ $zi == "0" ];then
         FCLIST[0]=`printf "%03d" ${ant100}`
      else
         FCLIST=("${FCLIST[@]}" "${ant100}" )
      fi
      echo "FCLIST="  "${FCLIST[@]}" 
      echo "    "
      echo "Forecast step = $ant"
      date_list=$(eval echo "\`date '+%Y%m%d%H' --date="\"${datein} ${ant3}:00:00 UTC - ${ant} hours\"" \`")
      echo "corresponding date = $date_list"
      date_run=${date_list:0:8}
      echo "run date = $date_run"
     
      #-----------------------------------------
      # Loop over previous forecasts
      #----------------------------------------
      # Retrieve lead time in hours and days and filename
      ant_nodays=$(printf "%02d" $(( ant / 24 )))
      ant_nohours=$(printf "%02d" $(( ant  % 24 )))
      afc0=`printf "%02d" $ant`
      echo "forecast step = " ${afc0}  $ant_nodays $ant_nohours
      FILE0="${PATH_FC}/${fispre}_${date_run}0000_+${ant_nodays}${ant_nohours}0000.grb"
      echo $FILE0
      if [ -s $FILE0 ];then
         echo "Start merging forecast files for run " $date_run
         fistmp="${WORKDIR}/tmp/filein_fcday-${zi}00.grib" 
         #------------------------------------------------------
         # Mandatory name for model file, contains vv"%03d%s" ${ant} 00
         #------------------------------------------------------
         fisfin=`printf ${WORKDIR}/input/${EXP}_fcday-vv"%03d%s" ${ant} 00.grib`
         cp $FILE0 $fistmp
         #----------------------------------
         # Check if reference fg_file set
         #----------------------------------
         if [ ${ranset} == "0" ]; then
            echo "Required field for reference atmosphere NOT set yet, create link"
            echo "Linking file for reference atmosphere = " ${FILE0}
            ln -s ${FILE0} ${WORKDIR}/input/fg_file
            echo "Required field for reference atmosphere set"
            ranset=1
         fi
         #-----------------------------------------------
         # Merge forecast file with steps -3 -6 -9 -12 -24, for cumulated fields
         #-----------------------------------------------
         for j in 3 6 9 12 24;do
            if [ $j -le ${ant} ];then 
               afc3=`printf "%02d" $(($ant - ${j}))`
               afc3_nodays=$(printf "%02d" $(( afc3 / 24 )))
               afc3_nohours=$(printf "%02d" $(( afc3  % 24 )))
               echo "forecast step-${j} = " ${afc3}
               if [ -s $FILE3 ];then
                  FILE3="${PATH_FC}/${fispre}_${date_run}0000_+${afc3_nodays}${afc3_nohours}0000.grb"
                  cat $FILE3 >> $fistmp
               else
                  echo "Missing forecast step-${j}"
               fi
            fi
         done
      else
         echo "Missing forecast file for step " ${afc0}
         echo "\n Moving to next forecast step"
      fi

      echo "    "
      echo "fistmp=" ${fistmp}
      echo "fisfin=" ${fisfin}
      ln -fs ${fistmp} ${fisfin}
      echo ${fisfin}
      echo "File for forecast step $ant ready"
   else
      echo "Forecast step ${ant} outside forecast range ${nohfc}"
   fi
done
echo "     "
echo "Forecast merging done"
echo "Forecast steps to verify " ${FCLIST[*]}
echo "     "
echo "     "
echo "     "
#-----------------------------------------------------------------------------
# PART II: Prepare MEC namelist
#-----------------------------------------------------------------------------
echo "\n Prepare namelist for MEC"
#-----------------------------------------------------------------------------
nmlfile="${PATH_CONST}/nml_template"
cp ${nmlfile} $WORKDIR/run/namelist

sed -i "~s~START_DATE~${date3}~g;~s~INAME~${EXP}~g;~s~TO_FILL~ ${FCLIST[*]}~g;~s~MODPREF~${fispre}~g" namelist
echo "Namelist prepared"
echo "     "
echo "     "
echo "     "
#-----------------------------------------------------------------------------
# PART III: RUN MEC
# Running sequence must be adapted to your local machine!
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# MODIFY ACCORDING TO LOCAL SPECIFICATIONS 
# START LOCAL MODIFICATIONS
#-----------------------------------------------------------------------------
echo "Adapt for your machine!"
echo "\n Run MEC"
pwd

parcomp_mpirun $BIN

# END LOCAL MODIFICATIONS
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Move output (feedback files) to run directory
cd $WORKDIR/output
for i in ${PROUT}*.nc; do
  mv -v ${i}* $WORKDIR/run/`basename $i .nc`.${date3}
done

# Clean-up
rm -f $WORKDIR/tmp/*
#-----------------------------------------------------------------------------

exit 0

