#!/usr/bin/env python3
# encoding: utf-8

import argparse, sys
from netCDF4 import Dataset  # http://code.google.com/p/netcdf4-python/

def command_line():
    parser = argparse.ArgumentParser(description = 'MODIFY NETCDF VARIABLE.\
                        The script read ekf files and save some information for each  \
                        observation report, which can be plot with plot_ekf.py ')
    parser.add_argument('--path',   default = ".",   type = str,
                        help = 'Folder in which ekf files are stored, contained in    \
                                in subdirectories named as analysis date and time in  \
                                the format YYYYMMDDHH defalut: current directory)')
    parser.add_argument('--variable',  default = "boh",  type = str,
                        help = 'Blank space separated list of observation reports to  \
                                analyze (default: AIREP SYNOP TEMP)')
    parser.add_argument('--value', default = "boh",  type = str, 
                        help = 'Blank space separated list of observation reports to  \
                                analyze (default: AIREP SYNOP TEMP)')
    return parser.parse_args()

# Read variables from commad line
args                  = command_line()
path, variable, value = args.path, args.variable, args.value

# Open NetCDF file
print("\nOpen file: %s" %path)
try:
    nc_fid = Dataset(path, 'r+')
except:
    sys.exit("File does not exist!")

# Check if variable exist
list_var = nc_fid.variables.keys() 
if variable not in list_var:
    sys.exit("ERROR! Variable %s is not in this file!" %variable)

# Modify variable
var_read = nc_fid.variables[variable][:]
print("Variable %s is %s. It will be set to %s" %(variable, var_read, value))
nc_fid.variables[variable][:] = value

nc_fid.close()
