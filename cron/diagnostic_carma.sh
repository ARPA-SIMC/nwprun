#!/bin/bash
. ~/.nwpconf

# -------------------------------------------------------------------------------------------
# DESCRIPTION - Updated 08/09/2021
# -------------------------------------------------------------------------------------------
# Given an input "reference" date, the script checks that all necessary files to run MEC are
# present and that verification files are actually created. This means that it checks that
# the following files are present:
#   - COSMO-2I lfff* files of the day before the reference date (at 00 UTC);
#   - observations downloaded from CARSA's FTP server of 4 days before the reference date;
#   - ver*.nc files  every 3 hours (i.e. at 00,03...21 UTC) for the same date of 
#        observations (i.e. 4 days before "reference" date).
# The reference date is provided as input parameter (from command line) in the format 
# YYYYMMDD;  if it is not provided, the current date is employed.
# In case any file is missing, an email is sent to the designated recipients
#
# Parameters and input variables:
#   - param $1          = reference date; if not provided, the current date is employed
#   - FOLD_COSMO_2I_SCR = folder in which COSMO-2I lfff* files are copied
#   - FOLD_OBS          = folder in which observations are downloaded 
#   - FOLD_OUT          = folder containing verification ver*.nc files, supposed to be in:
#                           $FOLD_OUT/YYYYMMDDhh/EXP/run
#                         where the date is automatically computed,  EXP can be any string
#                         (if you are running more than one experiment, this must to be 
#                         modified)
#   - EMAILS            = array of email address s to which to send an email in case of
#                         any problems. 


# -------------------------------------------------------------------------------------------
# MAIN
# -------------------------------------------------------------------------------------------
# input variables 
FC_LENGTH=72
FOLD_ICON_2I=$WORKDIR_BASE/CARMA/ICONDATA
FOLD_OBS=$WORKDIR_BASE/CARMA/COMMON_L
FOLD_OUT=$WORKDIR_BASE/CARMA/OUTPUT
EMAILS=("${EMAIL_ADDRESS[@]}")

# Define error array
error=()

# Get date
echo ""
if [ -z "$1" ]; then
    DATE=$(date -u --date "`date +%Y%m%d` -5 days" "+%Y%m%d")
    echo "No argument supplied. Use 5 days ago date: " $DATE
else
    DATE=$1
    echo "Input date: "$DATE
fi

# Check ICON-2I forecast files for DATE and previous forecast days that intersect DATE
FC_LENGTH_DAYS=$(( (FC_LENGTH + 23) / 24 ))
for dd in `seq 0 $FC_LENGTH_DAYS`; do
    # Retrieve forecast initialization date. Time is supposed to be 00 UTC. 
    ini_date=$(date -u --date "$DATE -$dd days" "+%Y%m%d")
    ini_time=00

    # Check expected number of files
    num_files=`ls $FOLD_ICON_2I/icon2I_${ini_date}${ini_time}00_*.grb 2> /dev/null |wc -l`

    if [ $num_files != 25 ]; then
        error_lfff=" - ICON-2I      run ${ini_date}  : ERROR $num_files instead of 25 files. "
        error_lfff+="    Check folder $FOLD_ICON_2I"
        error+=("$error_lfff")
        echo "$error_lfff"
    else
        echo " - ICON-2I      run ${ini_date}  : ok"
    fi
done

# Nice printing
if [ ${#error[@]} -ne 0 ]; then
    error+=("")
    echo ""
fi

# Check observations for DATE
num_obs_fold=`ls -d $FOLD_OBS/$DATE* 2> /dev/null |wc -l`
if [ $num_obs_fold != 8 ]; then
    error_obs=" - Observations for $DATE  : ERROR $num_obs_fold instead of   8 folders. "
    error_obs+=" Check folder $FOLD_OBS"
    error+=("$error_obs")
    echo "$error_obs"
else
    echo " - Observations for $DATE  : ok"
fi

# Nice printing
if [ ${#error_obs[@]} -ne 0 ]; then
    error+=("")
    echo ""
fi

# Crech ver* files for 4 days before the reference date
for hh in `seq -w 0 3 21`; do
    dataver=${DATE}${hh}
    num_ver=`ls $FOLD_OUT/$dataver/icon2I/run/ver* 2> /dev/null |wc -l`
    if [ $num_ver -eq 0  ]; then
        error_ver=" - Verification for $dataver: ERROR 0 instead of 1-3 files. "
        error_ver+="   Check folder $FOLD_OUT/$dataver"
        error+=("$error_ver")
        echo "$error_ver"
    else
        echo " - Verification for $dataver: ok ($num_ver files)"
    fi 
done

# Send email in case o any problems
if [ ${#error[@]} != 0 ]; then
    echo " Send email to report errors"

    # Save message in mail.txt file
    error_new=("The following errors were detected:")
    error_new+=("${error[@]}")
    printf "%s\n" "${error_new[@]}" > mail.txt

    # Define title and send email
    title=`echo 'CARMA diagnostics: some problems for ' "$DATE"`
    echo "$(cat mail.txt)" | mailx -s "$title" -S sendwait ${EMAILS[@]}
    rm mail.txt
fi

