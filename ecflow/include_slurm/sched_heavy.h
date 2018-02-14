#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
#SBATCH --ntasks=1
#SBATCH --mem=64G
#SBATCH --time=00:10:00
. ~/smnd_profile

module load autoload
module load intel
module load hdf5
module load intelmpi
module load mkl
export LD_LIBRARY_PATH=$WORK/srcintel/install/lib:$LD_LIBRARY_PATH
