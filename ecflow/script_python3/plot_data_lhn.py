'''
Script that performs the following tasks:
- read the CSV file containing the information on latent heat nudging (LHN)
- plot the info over the latest 24 hours, latest 30 days, and lates 365 days.

-------
Historia:
a.d. xv Kal. Iulias anno MMDCCLXXVII A.U.C. (17-06-2024) Adefonsus fecit.
'''
import argparse
import datetime as dt
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import os
import pandas as pd
from subprocess import Popen, PIPE

# ------------------------------------------------------------------------------
# PARAMETERS
# =====
# Paths
# =====
# Path to file containing the last date analized
LAST_DATE_FPATH = os.path.join(os.environ["WORKDIR"], 'last_date_lhn.txt')

# Name of the CSV file
CSV_FNAME_LHN = "diagnostic_LHN.csv"
# Directory where the CSV file is stored
CSV_DIR_LHN = os.environ["WORKDIR"]

# Directory where the plots are stored
PLOT_DIR = os.environ["WORKDIR"]

# ================
# Input parameters
# ================
# The format of date and time
DT_FORMAT = '%Y%m%d%H'

# ===============
# Plot parameters
# ===============
DPI = 150
# ------------------------------------------------------------------------------

def read_command_line_args():
    '''
    Read command line arguments

    Returns
    -------
    argparse.Namespace
        Object containing parsed arguments.

    '''
    parser = argparse.ArgumentParser(description="Plot LHN diagnostics")
    parser.add_argument('--end_date',
                        default = dt.datetime.now(dt.timezone.utc).strftime(DT_FORMAT),
                        type=str,
                        help='The last date considered by the script. The date is \
                              expected in the format YYYYMMDD. Default: date of today')
    parser.add_argument('--csv_folder',
                        default=CSV_DIR_LHN,
                        type=str,
                        help='The folder where the CSV file is stored. Default: \
                             %s' % CSV_DIR_LHN)
    return parser.parse_args()


def read_data(csv_fpath):
    """
    Read and parse the CSV file containing LHN info.

    Parameters
    ----------
    csv_fpath : str

    Returns
    -------
    pandas.DataFrame
        DataFrame with all data in the CSV file.
    
    """
    # Note: if we update Pandas, we should use:
    # date_format=DT_FORMAT
    # instead of date_parser
    return pd.read_csv(csv_fpath, parse_dates=['datetime'],
                       date_parser=lambda x: dt.datetime.strptime(x, DT_FORMAT))
                       


def filter_data(df, end_date, period):
    """
    Read and parse the CSV file containing LHN info.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame with all data in the CSV file.
    end_date : numpy.datetime64
        The last date to consider for the plot (format specified by DT_FORMAT).
    period : int
        Period in days over which data should be collected.

    Returns
    -------
    pandas.DataFrame
        Updated DataFrame with appended data.
    
    """
    start_date = end_date - dt.timedelta(days=period)
    return df[(df['datetime'] >= start_date) & (df['datetime'] <= end_date)]


def plot_summary(df, end_date, period, save_path):
    """
    Plots the LHN info for the selected period.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame with all data in the CSV file.
    end_date : numpy.datetime64
        The last date to consider for the plot (format specified by DT_FORMAT).
    period : int
        Period in days over which data should be plotted.
    save_path : str
        Full path where the plots will be stored.
    
    """
    # Setting up some parameters
    if period <= 1:
        freq = 'H'
        bar_width = 1./24. # days
    else:
        freq = 'D'
        bar_width = 1. # days
    bar_alpha = 1.
    
    # Resampling data
    data = df.set_index('datetime').resample(freq).mean().reset_index() # da controllare

    # Setting up panels
    fig, axs = plt.subplots(nrows=4, ncols=1, figsize=(12, 10))

    # Panel 1: available timesteps
    ax0 = axs[0]
    ax0.bar(data['datetime'], data['percentage of available timesteps'], width=bar_width,
             color='tab:gray', alpha=bar_alpha) #, label='# Available timesteps')
    ax0.set_ylabel('% of available timesteps')
    ax0.set_title(f'Percentage of available timesteps')
    ax0.set_ylim([-5., 105.])

    # Panel 2:  n of points with full spatial weight
    ax1 = axs[1]
    ax1.bar(data['datetime'], data['n of points with full spatial weight : numfull'], width=bar_width,
            color='tab:purple', alpha=bar_alpha) #, label='# w. full spatial weight')
    ax1.set_ylabel('Num. of points')
    ax1.set_title('Number of points with full spatial weight')

    # Panel 3: n of points with increments
    ax2 = axs[2]
    ax2.bar(data['datetime'], data['n of points with increments'], width=bar_width,
            color='tab:green', alpha=bar_alpha) #, label='# w. increments')
    ax2.set_ylabel('Num. of points')
    ax2.set_title('Number of points with increments')

    # Panel 2: up/down scaling
    ax3 = axs[3]
    ax3.bar(data['datetime'], data['n of points with upscaling'], width=bar_width,
            color='tab:blue', alpha=bar_alpha, label='Upscaling')
    ax3.bar(data['datetime'], data['n of points with limited upscaling'], width=bar_width,
            color='lightblue', alpha=bar_alpha, label='Limited Upscaling')
    ax3.bar(data['datetime'], -data['n of points with downscaling'], width=bar_width,
            color='tab:red', alpha=bar_alpha, label='Downscaling')
    ax3.bar(data['datetime'], -data['n of points with limited downscaling'], width=bar_width,
            color='sandybrown', alpha=bar_alpha, label='Limited Downscaling')
    ax3.set_ylabel('Num. of points')
    ax3.set_title('Number of points with up/down scaling')
    
    # Centering y axis around 0 and making labels positive
    max_y = data[['n of points with upscaling',
                  'n of points with limited upscaling',
                  'n of points with downscaling',
                  'n of points with limited downscaling']].values.max()*1.05
    ax3.set_ylim([-max_y, max_y])
    ax3.yaxis.set_major_formatter(lambda x, pos: f'{abs(x):g}')

    # Final adjustments to the axes
    start_date = end_date - dt.timedelta(days=period)
    for i_ax, ax in enumerate(axs):
        ax.set_axisbelow(True)
        ax.grid(ls=':', c='gray', alpha=0.5)
        ax.set_xlim([start_date, end_date])
        if i_ax < len(axs) - 1:
           ax.set_xticklabels([]) 

    # Formatting date on the x axis
    if period <= 1:
        ax3.xaxis.set_major_formatter(mdates.DateFormatter('%d-%b-%Y\n%H:%M'))
    elif period <= 30:
        ax3.xaxis.set_major_formatter(mdates.DateFormatter('%d-%b-%Y'))
    else:
        ax3.xaxis.set_major_formatter(mdates.DateFormatter('%d-%b\n%Y'))

    # Title of figure
    if period <= 1:
        suptitle_str = 'LHN diagnostic - last %.0f hours' % (period*24)
    else:
        suptitle_str = 'LHN diagnostic - last %d days' % period
    plt.suptitle(suptitle_str, fontweight='bold')

    # Legend
    plt.tight_layout()
    plt.subplots_adjust(bottom=0.08)
    fig.legend(loc='lower center', ncol=4)

    plt.savefig(save_path, dpi=DPI)
    plt.close()


def main():
    # Read variables from commad line
    args = read_command_line_args()
    csv_folder = args.csv_folder
    end_date = args.end_date

    # Reading data
    csv_fpath = os.path.join(csv_folder, CSV_FNAME_LHN)
    if os.path.exists(csv_fpath):
        if 1:
            df = read_data(csv_fpath)
        else:
            print(f"Could not read CSV file at: {csv_fpath}.")
            print('No plot will be produced.')
            return 2

    else:
        print(f"CSV file was not found at path: {csv_fpath}.")
        print('No plot will be produced.')
        return 1
    
    # Check last date in the CSV
    csv_end_date = df['datetime'].values[-1]
    selected_end_date = pd.to_datetime(end_date, format=DT_FORMAT)

    if csv_end_date < selected_end_date:
        # If this date differ from the last_date one, print a warnign, and trust the CSV
        print('Selected last date (%s) il later than from the one in %s (%s).' % (str(selected_end_date), 
                                                                                  os.path.basename(csv_fpath),
                                                                                  str(csv_end_date)))
        print('Using %s.' % str(csv_end_date))
        selected_end_date = csv_end_date
    selected_end_date = pd.to_datetime(selected_end_date)

    # Plotting
    periods = [1, 30, 365]
    for period in periods:
        filtered_df = filter_data(df, selected_end_date, period)
        if period <= 1:
            save_path = f"{PLOT_DIR}/LHN_last{period*24}h.png"
        else:
            save_path = f"{PLOT_DIR}/LHN_last{period}d.png"
        plot_summary(filtered_df, selected_end_date, period, save_path)


if __name__ == '__main__':
    main()
