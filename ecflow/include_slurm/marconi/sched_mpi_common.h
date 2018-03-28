#SBATCH --ntasks-per-node=32
#SBATCH --sockets-per-node=2
#SBATCH --cores-per-socket=16
#SBATCH --ntasks-per-socket=16
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=%TASK_PER_CORE%

module load autoload
module load intel
module load hdf5
module load intelmpi
module load mkl
export LD_LIBRARY_PATH=$WORK/srcintel/install/lib:$LD_LIBRARY_PATH
