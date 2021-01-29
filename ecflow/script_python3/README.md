#### KENDA DIAGNOSTICS
# Required packages:
The python3 scripts "read_ekf.py" and "plot_ekf.py", launched by "diagnostic_ekf.ecf" 
require the following python3 modules:
- matplotlib
- argparse
- datetime
- pandas
- netcdf4-python
Note that "python3-tk" must be installed. To verify if it is installed, open your
Python console and type: "import tkinter as tk"

# Installation of python3 packages on Cineca HPC systems
For installing and using these modules on Cineca HPC systems, you need to use the 
“virtualenv”/"pyvenv" tool following these instructions.
1) Load the python3 module:
      module load python                # check that version is 3 (>= 3.8 on Meucci)
2) Create a virtualenv, basically just a new directory (my_venv) containing all you need:
      virtualenv env_python3            # Galileo
      python3 -m venv env_python3       # Meucci
3) Activate the new virtualenv
      source env_python3/bin/activate
4) Install what you need (ex. "pip install matplotlib") or launch s script which 
   uses already installed python3 packages
5) Deactivate the virtualenv when you are done working
      deactivate 

# Step by step installation of required python3 packages
Installation of "matplotlib", "argparse", "datetime", "pandas" can be done via pip:
      pip install matplotlib
      pip install argparse
      pip install datetime
      pip install pandas

Regarding "netcdf4-python", follow these instruction:
1) Download the package from: https://github.com/Unidata/netcdf4-python
3) Install "Cython", "setuptools" and "cftime" python3 packages, that is:
      pip install Cython
      pip install setuptools
      pip install cftime
3) Load required modeules: 
      module load autoload
      module load intel
      module load hdf5
      module load intelmpi
      module load mkl
      module load netcdf
      . ~/smnd_profile
   On Galielo pay attntion to load a 'netcdf' module which not creates conflits.
   Note that netcdf must be load but it is not necessary to do so when you use 
   the package. 
4) Enter netcdf4-python folder and wxwxute
5) Modify "setup.cfg" to specify the correct value for "HDF5_dir", which should
   correnspond to $HDF5_HOME
6) Launch:
      python setup.py build
      python setup.py install
    
