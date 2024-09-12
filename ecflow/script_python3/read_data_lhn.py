'''
Script that performs the following tasks:
- read the log files containing information on latent heat nudging (LHN)
- write the information that needs to be plotted into a CSV file.

-------
Historia:
a.d. v Non. Maias anno MMDCCLXXVII A.U.C. (03-05-2024) Adefonsus fecit.
a.d. xv Kal. Iulias anno MMDCCLXXVII A.U.C. (17-06-2024) Adefonsus mutavit.
'''
import argparse
import collections
import copy
import datetime as dt
import glob
import numpy as np
import os
import pandas as pd
from itertools import islice
from subprocess import Popen, PIPE

# ------------------------------------------------------------------------------
# PARAMETERS
# =====
# Paths
# =====
# Path to file containing the last date analized
LAST_DATE_FPATH = os.path.join(os.environ["WORKDIR"], 'last_date_lhn.txt')

# Name of the LHN log file
LOG_FNAME = 'lhn.log'

# Name of the file where the output will be saved
CSV_FNAME_LHN = "diagnostic_LHN.csv"
# Directory where the file above is stored
CSV_DIR_LHN = os.environ["WORKDIR"]

# ================
# Input parameters
# ================
# String associated to time information
TIME_INFO_STR = "Diagnostics of LHN, lhn_t_inc, timestep"

# Strings with info to save
CHOSEN_STR_LIST = [
    'n of points with increments',
    'n of points with local profiles',
    'n of points with upscaling',
    'n of points with limited upscaling',
    'n of points with downscaling',
    'n of points with limited downscaling',
    'n of points with artif. prof',
    'n of points with full spatial weight : numfull'
]

# Max number of line to check for each time step
MAX_LINES_PER_SECTION = 33

# =================
# Output parameters
# =================
# Name of the columns in the dataframe containing date/time info
DT_COL_NAME = 'datetime'
NUM_TIMESTEPS_COL_NAME = 'n of available timesteps'
PERC_TIMESTEPS_COL_NAME = 'percentage of available timesteps'
NUM_FILES_COL_NAME = 'n of available logfiles'

# The format of date and time
DT_FORMAT = '%Y%m%d%H'
# ------------------------------------------------------------------------------

def consume(iterator, n):
    """
    Advance the iterator n-steps ahead.

    Parameters
    ----------
    iterator : iterable
        The iterable object to consume.
    n : int or None
        The number of steps to advance. If None, consume the entire iterator.

    Returns
    -------
    None

    Notes
    -------
    Function taken from:
    https://stackoverflow.com/questions/17837316/how-do-i-skip-a-few-iterations-in-a-for-loop

    """
    # Use functions that consume iterators at C speed.
    if n is None:
        # feed the entire iterator into a zero-length deque
        collections.deque(iterator, maxlen=0)
    else:
        # advance to the empty slice starting at position n
        next(islice(iterator, n, n), None)


def read_logfile(log_fpath, empty_data_dict):
    """
    Read and parse the LHN logfile containing time-stamped data.

    Parameters
    ----------
    log_fpath : str
        Path to the logfile.
    empty_data_dict : dict
        Dictionary whose keys are the column names, and values are arrays full of zeros.

    Returns
    -------
    tuple
        A tuple containing:
        - dict: A dictionary with selected logfile content as keys, each 
                containing a list of values for all time steps.
        - int: Number of time steps available in the log file.

    """
    # Initializing the dictionary with columns full of a chosen fill value
    data_dict = copy.deepcopy(empty_data_dict)

    # Open the file and read it line by line
    with open(log_fpath, "r") as file:
        lines = file.readlines()

        # Trick that will allow us to skip lines in the loop
        num_lines = len(lines)
        lines_iterable = iter(range(num_lines))

        # Counter for time steps
        time_step_idx = 0

        for i_line in lines_iterable:
            line = lines[i_line]
            # Check if the line contains the time information
            if TIME_INFO_STR in line:
                # Optional: extract the time step
                # time = float(line.split(":")[-1].strip())

                # Loop until the next section or max num lines
                for control_idx in range(1, MAX_LINES_PER_SECTION):
                    curr_idx = min(i_line + control_idx, num_lines -1)
                    line = " ".join(lines[curr_idx].split())
                    if ':' in line:
                        for info_str in CHOSEN_STR_LIST:
                            if info_str in line:
                                # Read data and store it into a dictionary
                                if '=' in line:
                                    value = int(line.split("=")[-1].strip())
                                else:
                                    value = int(line.split(":")[-1].strip())
                                data_dict[info_str][time_step_idx] = value
                    
                    # Break the loop if we reached the end of the section
                    if ("Verification" in line) or (TIME_INFO_STR in line):
                        break

                # Incrementing the time step counter
                time_step_idx += 1

                # Now skip ahead by the number of lines already checked
                consume(lines_iterable, control_idx - 1)
    return data_dict, time_step_idx


def include_logfile_in_df(df, logfile_dict, chosen_datetime, num_timesteps, perc_timesteps,
                          num_files, agregator_fun=np.nanmean, aggregate_unavailable_timesteps=False):
    """
    Include data from the dictionary created from a logfile into a DataFrame.

    Parameters
    ----------
    df : pandas.DataFrame
        The DataFrame to which the data will be added.
    logfile_dict : dict
        Dictionary containing logfile data.
    chosen_datetime : datetime
        Datetime for the logfile data.
    num_timesteps : int
        Number of time steps available in the logfile.
    perc_timesteps : float
        Percentage of available timesteps over expected number.
    num_files : int
        Number of logfiles for the current date/time (normally 1 or 0).
    aggregator_fun : function, optional
        Function to aggregate data, default is np.nanmean.
    aggregate_unavailable_timesteps : bool, optional
        If true, unavailable timesteps will be aggregated by the function

    Returns
    -------
    pandas.DataFrame
        Updated DataFrame with appended data.
    
    """
    dict_aggregated = {DT_COL_NAME: [chosen_datetime]}
    if num_timesteps == 0:
        # If no data is available, we just write a zero
        for info_str in CHOSEN_STR_LIST:
            dict_aggregated[info_str] = [0]
    else:
        # The flag aggregate_unavailable_timesteps controls wheter mising data are aggregated
        if aggregate_unavailable_timesteps:
            last_idx_aggregation = -1
        else:
            last_idx_aggregation = num_timesteps
        for info_str in CHOSEN_STR_LIST:
            dict_aggregated[info_str] = [agregator_fun(logfile_dict[info_str][:last_idx_aggregation])]
    dict_aggregated[NUM_TIMESTEPS_COL_NAME] = [num_timesteps]
    dict_aggregated[PERC_TIMESTEPS_COL_NAME] = [perc_timesteps]
    dict_aggregated[NUM_FILES_COL_NAME] = [num_files]
    return pd.concat([df, pd.DataFrame.from_dict(dict_aggregated)], ignore_index = True)


def date_range_iterator(start_date_str, end_date_str, lencyc):
    """
    Generate a range of dates as strings between two given dates.

    Parameters
    ----------
    start_date_str : str
        Start date in the format "%Y%m%d".
    end_date_str : str
        End date in the format "%Y%m%d".
    lencyc : int
        Length of the assimilation cycle in hours.

    Yields
    ------
    str
        Date string in the format indicated by DT_FORMAT (normally "%Y%m%d%H").

    Notes
    -----
    The iterator starts at the first hour of the day after start_date_str, and
    ends at the hour 23:00 of end_date_str.

    """
    start_date = dt.datetime.strptime(start_date_str, DT_FORMAT) + dt.timedelta(hours=1)
    end_date = dt.datetime.strptime(end_date_str, DT_FORMAT)

    current_date = start_date
    while current_date <= end_date:
        yield current_date.strftime(DT_FORMAT)
        current_date += dt.timedelta(hours=lencyc)


def read_command_line_args():
    '''
    Read command line arguments

    Returns
    -------
    argparse.Namespace
        Object containing parsed arguments.

    '''
    parser = argparse.ArgumentParser(description = 'LHN DIAGNOSTIC - READ LOG FILES.\
                        The script read lhn.log files and save some information for each\
                        analysis date and time, which can be plot with plot_data_lhn.py')
    parser.add_argument('--folder',
                        default = os.environ["LETKF_ARCHIVE"],
                        type = str,
                        help = 'Folder in which lhn.log files are stored, contained in  \
                                in subdirectories named as analysis date and time in  \
                                the format YYYYMMDDHH. Defalut: current directory')
    parser.add_argument('--end_date',
                        default = dt.datetime.now(dt.timezone.utc).strftime(DT_FORMAT),
                        type = str,
                        help = 'The last date considered by the script. The date is \
                                expected in the format YYYYMMDD. Default: date of today')
    parser.add_argument('--lencyc',
                        default = 1,
                        type = int,
                        help = 'Length of assimilation cycles in hours. Default: 1')
    parser.add_argument('--leniau',
                        default = 5,
                        type = int,
                        help = 'Length of incremental analysis update in minutes. Default: 5')
    parser.add_argument('--timestep',
                        default = 20,
                        type = int,
                        help = 'Length of model timestep in seconds. Default: 20')
    return parser.parse_args()


def main():
    # Read variables from commad line
    args = read_command_line_args()
    folder = args.folder
    end_date_str = args.end_date
    lencyc = args.lencyc
    leniau = args.leniau
    timestep = args.timestep

    # Defining the output CSV path
    csv_fpath = os.path.join(CSV_DIR_LHN, CSV_FNAME_LHN)

    # Computing number of expected LHN steps
    num_steps_iau = (leniau * 60. / timestep) - 1
    num_steps_cyc = lencyc * 3600. / timestep
    num_expected_steps = int(num_steps_iau + num_steps_cyc + 1)

    # Allocating empty array with correct number of steps
    empty_data_dict = {}
    for k in CHOSEN_STR_LIST:
        empty_data_dict[k] = np.full(num_expected_steps, 0, dtype=int)

    # The available directories (for selecting "end" and eventually "start" date-time)
    available_dir_list = sorted(glob.glob(os.path.join(folder,
                                '[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-1][0-9]')))
    if not len(available_dir_list):
        print('No LHN diagnostic directory available.')
        return 1
    last_available_date_str = os.path.basename(available_dir_list[-1])
    if int(end_date_str) > int(last_available_date_str):
        print('Selected end date (%s) after last available date (%s).' % (end_date_str, last_available_date_str))
        end_date_str = last_available_date_str

    # Selecting first date to analyze
    if os.path.isfile(LAST_DATE_FPATH) and os.path.getsize(LAST_DATE_FPATH):
        # If it is available in a file we use it
        with open(LAST_DATE_FPATH, 'r') as date_f:
            ini_date_str = date_f.readline()
        # Check if it is the same date as in the CSV
        if os.path.exists(csv_fpath):
            # Get the last line from the file
            p = Popen(['tail', '-1', csv_fpath], shell=False, stderr=PIPE, stdout=PIPE)
            res,err = p.communicate()
            if err:
                print('Error while getting last line of %s.' % csv_fpath)
                print(err.decode())
            else:
                # Use split to get the part of the line that you require
                csv_date = res.decode().split(',')[0]
                if ini_date_str != csv_date:
                    # If this date differ from the last_date one, print a warnign, and trust the CSV
                    print('CSV last date (%s) differs from the one in %s (%s).' % (csv_date, 
                                                                                   os.path.basename(LAST_DATE_FPATH),
                                                                                   ini_date_str))
                    print('Using %s.')
                    ini_date_str = csv_date
        print("Last date in file '%s':%s \n" % (LAST_DATE_FPATH, ini_date_str))
    else:
        # If this file is not available, we look for the oldest date in folder
        ini_date = dt.datetime.strptime(os.path.split(available_dir_list[0])[1],
                                        DT_FORMAT) - dt.timedelta(hours=1)
        ini_date_str = ini_date.strftime(DT_FORMAT)
        print(("File '%s' does not exixt. All folders in '%s' will be " + \
               "analyzed. First date: %s \n") % (LAST_DATE_FPATH, folder,
               (ini_date + dt.timedelta(hours=1)).strftime(DT_FORMAT)))

    if float(ini_date_str) >= float(end_date_str):
        print('Start date (%s) coincides or is later than the end date (%s).' % (ini_date_str, end_date_str))
        print('Nothing has been added to the output files.')
        return 0
    
    # Generate the names of the folder that potentially contain log files
    dir_names = date_range_iterator(ini_date_str, end_date_str, lencyc)

    # Creating the empty dataframe that will contain all the loaded data
    df_columns = [DT_COL_NAME] + CHOSEN_STR_LIST + \
                    [NUM_TIMESTEPS_COL_NAME, PERC_TIMESTEPS_COL_NAME, NUM_FILES_COL_NAME]
    df = pd.DataFrame(columns=df_columns)

    # Read the data from all available logfiles
    for dirname in dir_names:
        print(dirname)
        log_fpath = os.path.join(folder, dirname, LOG_FNAME)
        # Series of checks: load data, or return zero values
        if os.path.isfile(log_fpath):
            if 1:
                logfile_dict, num_timesteps = read_logfile(log_fpath, empty_data_dict)
                num_files = 1
            else:
                print("Could not read file '%s'." % log_fpath)
                logfile_dict, num_timesteps, num_files = empty_data_dict, 0, 0
        else:
            print("File '%s' does not exist." % log_fpath)
            logfile_dict, num_timesteps, num_files = empty_data_dict, 0, 0

        # Computing percentage of available timesteps
        perc_timesteps = 100. * num_timesteps / float(num_expected_steps)

        # Getting the date from the path
        curr_datetime = dt.datetime.strptime(dirname, DT_FORMAT)

        # Saving a single value per column per file in the dataframe
        df = include_logfile_in_df(df, logfile_dict, curr_datetime, 
                                   num_timesteps, perc_timesteps, num_files)

    # Writing the dataframe as CSV
    try:
        if os.path.isfile(csv_fpath):
            df.to_csv(csv_fpath, mode='a', header=False, index=False,
                      date_format=DT_FORMAT)
            print("Logfile content appended to '%s'." % csv_fpath)
        else:
            df.to_csv(csv_fpath, mode='w', header=True, index=False,
                      date_format=DT_FORMAT)
            print("Logfile content written to '%s'." % csv_fpath)
    except:
        print("Could not write results in file '%s'." % csv_fpath)
        return 1

    # Updating the file containing the last date
    with open(LAST_DATE_FPATH, mode='w') as date_f:
        date_f.write(curr_datetime.strftime(DT_FORMAT))
    
    return 0


if __name__ == '__main__':
    main()
