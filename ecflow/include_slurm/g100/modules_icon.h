if [[ "%SUITE%" == *icon_2I_enda* ]]; then
    #module load cdo
    module load intel/oneapi-2021--binary 
    module load intelmpi/oneapi-2021--binary 
    module load hdf5/1.10.7--intel--2021.4.0 
    module load netcdf-c/4.8.1--intel--2021.4.0 
    module load netcdf-fortran/4.5.3--intel--2021.4.0 
    module load szip/2.1.1--oneapi--2021.2.0-ifort 
    module load mkl/oneapi-2021--binary 
    module load eccodes/2.21.0--intel--2021.4.0
else
    module load intel-oneapi-compilers/2021.4.0
    module load intel-oneapi-mpi/2021.4.0
    module load hdf5/1.10.7--intel--2021.4.0
    module load netcdf-c/4.8.1--intel--2021.4.0
    module load netcdf-fortran/4.5.3--intel--2021.4.0
    module load szip/2.1.1--oneapi--2021.2.0-ifort
    module load intel-oneapi-mkl/2021.4.0
    module load eccodes/2.21.0--intel--2021.4.0
    module load cmake
fi
