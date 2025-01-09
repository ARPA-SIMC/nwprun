import os, sys, argparse, subprocess
from datetime import datetime, timedelta

"""
DESCRIPTION - 09/01/2025
This script checks for delays in analysis data production and detects significant 
variations in soil moisture using GRIB2 files. It performs two main checks:
  - Data delay check: Verifies that the most recent folder is no older than 6 hours.
  - Soil moisture variation check: Analyzes data from the last N hours (defined by --hours
    option) to detect significant soil moisture variations exceeding a given threshold
    (defined by --threshold option)
"""

def read_grib_value(grib_file):
    """
    Reads the GRIB2 file and extracts the average soil humidity for level 243.
    """
    command = [
        "grib_get",
        "-p", "average",
        "-w", "shortName=W_SO,scaledValueOfFirstFixedSurface=243",
        grib_file
    ]
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                universal_newlines=True, check=True)
        print(result.stdout)
        return float(result.stdout.strip())
    except subprocess.CalledProcessError as e:
        print(f"Error reading GRIB file {grib_file}: {e}")
        print(e.stderr)
        return None

def get_latest_folder(base_path):
    """
    Finds the latest folder in the specified base path based on its name.
    """
    try:
        folders = [f for f in os.listdir(base_path) if os.path.isdir(os.path.join(base_path, f))]
        latest_folder = max(folders, key=lambda x: datetime.strptime(x, "%Y%m%d%H"))
        return latest_folder
    except ValueError as e:
        print(f"Error parsing folder names: {e}")
        return None
    except Exception as e:
        print(f"Error finding latest folder: {e}")
        return None

def check_data_delay(base_path):
    """
    Checks if the latest folder in the base path is older than 6 hours.
    If it is, returns an alert message.
    """
    latest_folder_name = get_latest_folder(base_path)
    if not latest_folder_name:
        return "ERROR: No valid folders found."
    
    latest_folder_time = datetime.strptime(latest_folder_name, "%Y%m%d%H")
    now = datetime.utcnow()

    # Check if the latest folder is older than 6 hours
    if (now - latest_folder_time).total_seconds() > 6 * 3600:
        return f"ALERT: No recent data. Latest folder ({latest_folder_name}) is more than 6 hours old.\nCurrent UTC time: {now}\n"

    return None

def check_humidity_variation(base_path, hours_to_check, threshold):
    """
    Checks the humidity variation over the past `hours_to_check` hours.
    If any variation exceeds the threshold, returns an alert message.
    """
    latest_folder_name = get_latest_folder(base_path)
    if not latest_folder_name:
        return "ERROR: No valid folders found."

    latest_folder_time = datetime.strptime(latest_folder_name, "%Y%m%d%H")

    humidity_data = []
    for hour_offset in range(hours_to_check):
        folder_time = latest_folder_time - timedelta(hours=hour_offset)
        folder_name = folder_time.strftime("%Y%m%d%H")
        folder_path = os.path.join(base_path, folder_name)

        if not os.path.exists(folder_path):
            continue

        grib_files = [f for f in os.listdir(folder_path) if f.startswith("icon_") and f.endswith("_+00010000_diag_det.grb")]

        for grib_file in grib_files:
            grib_file_path = os.path.join(folder_path, grib_file)
            humidity = read_grib_value(grib_file_path)
            if humidity is not None:
                humidity_data.append((folder_time, humidity))

    alerts = []
    for i in range(1, len(humidity_data)):
        prev_time, prev_humidity = humidity_data[i - 1]
        curr_time, curr_humidity = humidity_data[i]

        variation = abs(curr_humidity - prev_humidity) / prev_humidity * 100
        if variation > threshold:
            alerts.append(
                (f"ALERT: Significant soil humidity variation detected.\n"
                 f"Analysis {prev_time}, Humidity: {prev_humidity:.2f}\n"
                 f"Analysis {curr_time}, Humidity: {curr_humidity:.2f}\n"
                 f"Variation: {variation:.2f}%\n")
            )

    return "\n".join(alerts) if alerts else "OK: No significant variations detected."

def command_line():
    parser = argparse.ArgumentParser(description = 'CHECK ANALYSIS                      \
                        Check for delays on analysis production and cold re-start.')
    parser.add_argument('--folder', default = ".",   type = str,
                        help = 'Path to the directory containing analysis data.')
    parser.add_argument('--hours', default = 12,     type = int,
                        help = 'Number of hours to consider for soil moisture variation \
                                analysis (default: 12).')
    parser.add_argument('--threshold', default = "2.0", type = float,
                        help = 'Percentage threshold for detecting significant soil     \
                                moisture variations (default: 2.0).')
    return parser.parse_args()

if __name__ == "__main__":
    # Read variables from command line
    args = command_line()
    folder, hours, threshold = args.folder, args.hours, args.threshold

    # Check for data delay and humidity variation
    delay_alert = check_data_delay(folder)
    humidity_alert = check_humidity_variation(folder, hours, threshold)

    # If there are any alerts, write them to a file
    alerts = []
    if delay_alert:
        alerts.append(delay_alert)
    if humidity_alert and "OK" not in humidity_alert:
        alerts.append(humidity_alert)

    if alerts:
        with open("alert_message.txt", "w") as f:
            f.write("\n".join(alerts))
