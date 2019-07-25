#SBATCH --ntasks-per-node=32
#SBATCH --sockets-per-node=2
#SBATCH --cores-per-socket=16
#SBATCH --ntasks-per-socket=16
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=%TASK_PER_CORE%
