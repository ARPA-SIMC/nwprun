#!/bin/bash
#PBS -o %ECF_JOBOUT%
#PBS -j oe
#PBS -l select=1:ncpus=1:mpiprocs=1:mem=64gb
#PBS -l place=pack:shared
#PBS -l walltime=00:10:00
. ~/smnd_profile

module load autoload
module load intel
module load hdf5
module load intelmpi
module load mkl
export LD_LIBRARY_PATH=$WORK/srcintel/install/lib:$LD_LIBRARY_PATH

