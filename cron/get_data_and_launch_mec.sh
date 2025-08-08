#!/bin/bash


# -------------------------------------------------------------------------------------------
# DESCRIPTION - Updated 01/08/2025
# -------------------------------------------------------------------------------------------
# This script manages the preparation of input data for running MEC on a given observation 
# date, provided as input ($1) in the format YYYYMMDD. If not specified, it defaults to 
# 5 days before the current UTC date.
#
# The observation date is considered as a 24-hour window (00 to 21 UTC, every 3 hours).
# For that window, the script:
#   - Searches for all ICON-2I forecasts initialized at 00 UTC on the observation day
#     and on previous days, whose forecast ranges intersect the observation date and times.
#   - Downloads the required ICON-2I files from a local Arkimet archive.
#   - Downloads compressed observational data from CARMA's FTP, extracts them, and renames
#     the folders according to observation validity time (YYYYMMDDHH).
#   - Submits a MEC job for each 3-hourly observation time (00 to 21 UTC) of the target day.
#
# Parameters and key variables:
#   - param $1        = observation date (format YYYYMMDD); defaults to 5 days ago if unset
#   - FC_LENGTH       = forecast length in hours
#   - FOLD_ICON_2I    = output directory for ICON-2I forecast files
#   - MODEL_ARKI_DS   = Arkimet dataset name for ICON-2I
#   - FOLD_OBS        = output directory for observations
#   - MEC_SCRIPT      = path to the script that launches MEC jobs


# -------------------------------------------------------------------------------------------
# DESCRIPTION - Updated 25/08/2021
# -------------------------------------------------------------------------------------------
# This script downloads lfff* files of COSMO-2I and observations from CARMA'S FTP server and 
# submit the job in which MEC is launched. The parameter in input defines the "reference 
# date" in the format YYYYMMDD; if it is not provided, the current date is employed.
# Given this "refererence date", COSMO-2I files of the previous day are copied and 
# observations of 4 days before are downloaded. The MEC script is launched for the same date
# of observations (i.e. 4 days before reference date) every 3 hours (i.e. at 00,03...21 UTC).
#
# Parameters and variables defined in MAIN section:
#   - param $1          = reference date; if not provided, the current date is employed
#   - FOLD_COSMO_2I_SCR = folder in which COSMO-2I lfff* files are copied
#   - FOLD_COSMO_2I     = folder in which the MEC script looks for lfff* files (here
#                         links to files in FOLD_COSMO_2I_SCR are created)
#   - FOLD_OBS          = folder in which observations are downloaded and where the MEC
#                         script looks for observations
#   - MEC_SCRIPT        = path of the MEC script


# -------------------------------------------------------------------------------------------
# FUNCTIONS
# -------------------------------------------------------------------------------------------
# The date and time as requested by reftime arki-query key
getarki_datetime() {
    if [ "${#1}" -gt 8 ]; then
        date -u --date "${1:0:8} ${1:8:4}" '+%Y-%m-%d %H:00'
    else
        date -u --date "$1 $2:00" '+%Y-%m-%d %H:00'
    fi
}

# Function get_icon2I
# This function retrieves, from a local arkimet archive, ICON-2I forecast data for a 
# specific observation day, assuming that observations are collected every 3 hours (00, 
# 03, 06... UTC). Input variables:
#  - param $1 = observation date (YYYYMMDD).
#  - param $2 = path in which files will be saved

get_icon2I() {
    # Get input variables
    local DATA=${1:0:8}
    local OUT_DIR=$2

    # Loop back in time for the length of the forecast approximated by excess to cover 
    # any forecasts involving a non-integer number of days
    FC_LENGTH_DAYS=$(( (FC_LENGTH + 23) / 24 ))
    for dd in `seq 0 $FC_LENGTH_DAYS`; do
        # Retrieve forecast initialization date. Time is supposed to be 00 UTC. 
        ini_date=$(date -u --date "$DATA -$dd days" "+%Y%m%d")
        ini_time=00

        # Loop over hours at which observations are provided
        for hh in $(seq -w 0 3 $FC_LENGTH); do
            # Define lead time in hours and days
            hh_dec=$((10#$hh))  # forza interpretazione decimale
            nodays=$(printf "%02d" $(( hh_dec / 24 )))
            nohours=$(printf "%02d" $(( hh_dec % 24 )))

            # Define output file name
            ofile=icon2I_${ini_date}${ini_time}00_+${nodays}${nohours}0000.grb
            echo "Download file " $ofile

            # Don't download file if it already exists
            if [ -s "$OUT_DIR/$ofile" ]; then
                echo "  - File already downloaded, skip to next"
                continue
            fi

            # Extract data
            reftime=`getarki_datetime $ini_date $ini_time`
            timerange="timerange:Timedef,${hh}h,254"
            $SIMC_TOOLS arki-query --data -o $OUT_DIR/$ofile \
                "reftime:=$reftime;$timerange;$MODEL_ARKI_PARAM" $MODEL_ARKI_DS

            # Retry after 5 seconds if file is empty
            if [ ! -s "$OUT_DIR/$ofile" ]; then
                echo "File $OUT_DIR/$ofile does not exist or is empty, retry"
                sleep 5
                $SIMC_TOOLS arki-query --data -o $OUT_DIR/$ofile \
                    "reftime:=$reftime;$timerange;$MODEL_ARKI_PARAM" $MODEL_ARKI_DS
            fi
        done
    done
}


# Function get_obs_from_ftp
# Download compressed observations from CARMA's FTP for the day provided in input as 1st 
# parameter. Data are saved in the path provided in input as 2nd parameter in folders named 
# as the validity date and time of observations. Parameters and local variables:
#  - param $1        = day of observations to be downloaded in format YYYYMMDD
#  - param $2        = path in which files will be copied in subfolders named as the
#                      validity date and time of observations, i.e.: $OUT_DIR/YYYYMMDDHH
# - local NCFTPAUTH =  configuration file to access the FTP server

get_obs_from_ftp() {

    local DATA=$1
    local OUT_DIR=$2
    local NCFTPAUTH=$HOME/.auth/get_obs_from_ftp.cfg

    # load ncftp module
    . /etc/profile
    module load ncftp

    # Move to output directory
    mkdir -p $OUT_DIR
    cd $OUT_DIR

    # Download data from ftp
    echo "" >&2
    echo "Download observations from FTP for " $DATA >&2
    echo "" >&2
    filename="obs_nc_$DATA.tar.gz"
    filepath="OBS_NC/$filename"
    ncftpget -V -f $NCFTPAUTH . $filepath
    
    # Extract data
    tar -xvf $filename
    for f in `ls -d OBS_CONVERT/*`; do
        foldname=`basename $f`
        fold_renamed=${foldname:0:10}
        if [ -d $fold_renamed ]; then 
            rm -r $fold_renamed
        fi
        mv $f $fold_renamed
    done
    
    # Clean-up and moving back to previous directory
    rm -r OBS_CONVERT $filename
    cd -
}


# -------------------------------------------------------------------------------------------
# MAIN
# -------------------------------------------------------------------------------------------
FC_LENGTH=72
FOLD_ICON_2I=$WORKDIR_BASE/CARMA/ICONDATA
MODEL_ARKI_DS=$WORKDIR_BASE/arkimet/icon_2I_unstr
FOLD_OBS=$WORKDIR_BASE/CARMA/COMMON_L
MEC_SCRIPT=$WORKDIR_BASE/CARMA/MEC_SCR/run_mec_template.sh

# Get date
echo ""
if [ -z "$1" ]; then
    DATE=$(date -u --date "`date +%Y%m%d` -5 days" "+%Y%m%d")
    echo "No argument supplied. Use 5 days ago date: " $DATE
else
    DATE=$1
    echo "Input date: "$DATE
fi

# Get ICON-2I forecast files which matches the observation date (DATE)
get_icon2I $DATE $FOLD_ICON_2I

# Get observations
get_obs_from_ftp $DATE $FOLD_OBS

# Touch blaclist file for observations
touch  $FOLD_OBS

# Launch mec
for hh in `seq -w 0 3 21`; do
    sbatch --output="/ind2/meteo/a07smr03/lami/CARMA/log/${DATE}${hh}.log" \
           --error="/ind2/meteo/a07smr03/lami/CARMA/log/${DATE}${hh}.log"  \
            $MEC_SCRIPT ${DATE}${hh} $FC_LENGTH
done

# Clean-up input files older than 30 days from DATE 
cutoff_date=$(date -u --date="$DATE -30 days" +%Y%m%d)
echo "Cleaning observation folders and forecast files older than $cutoff_date..."

# Clean FOLD_OBS (directories named YYYYMMDDHH)
for d in "$FOLD_OBS"/??????????; do
    dir_name=$(basename "$d")
    dir_date=${dir_name:0:8}
    if [[ "$dir_date" < "$cutoff_date" ]]; then
        echo "Deleting OBS folder: $d"
        rm -rf "$d"
    fi
done

# Clean FOLD_ICON_2I (files named icon2I_YYYYMMDD....grb)
for f in `ls $FOLD_ICON_2I/icon2I_*.grb`; do
    fname=$(basename "$f")
    file_date=${fname:7:8}  # YYYYMMDD in position 7–14
    if [[ "$file_date" < "$cutoff_date" ]]; then
        echo "Deleting ICON-2I file: $f"
        rm -f "$f"
    fi
done

echo "Cleanup complete."

