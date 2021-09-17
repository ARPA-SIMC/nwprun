module load autoload
module load intel
#module load hdf5/1.10.7--intelmpi--oneapi-2021--binary
module load intelmpi
module load mkl
export LD_LIBRARY_PATH=$WORKDIR_BASE/srcintel/install/lib:$LD_LIBRARY_PATH
unset I_MPI_PMI_LIBRARY
