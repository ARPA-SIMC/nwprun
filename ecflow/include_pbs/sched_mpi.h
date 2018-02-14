#!/bin/bash
#PBS -o %ECF_JOBOUT%
#PBS -j oe
#PBS -l select=%NNODES_MODEL%:ncpus=32:mpiprocs=32:mem=118gb
#PBS -l walltime=%WALL_TIME%

module load autoload
module load intel
module load hdf5
module load intelmpi
module load mkl
export LD_LIBRARY_PATH=$WORK/srcintel/install/lib:$LD_LIBRARY_PATH

