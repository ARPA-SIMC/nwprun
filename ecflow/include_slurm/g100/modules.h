module load autoload
module load intel-oneapi-compilers/2021.4.0
module load intel-oneapi-mpi/2021.4.0
module load intel-oneapi-mkl
#module load hdf5/1.10.7--intelmpi--oneapi-2021--binary
export LD_LIBRARY_PATH=$WORKDIR_BASE/srcintel/install/lib:$LD_LIBRARY_PATH
#unset I_MPI_PMI_LIBRARY
