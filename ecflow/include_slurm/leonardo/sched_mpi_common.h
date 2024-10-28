#SBATCH --partition=dcgp_usr_prod
##SBATCH --reservation meteo_chains
#SBATCH --ntasks-per-node=112
#SBATCH --sockets-per-node=2
##SBATCH --cores-per-socket=56
#SBATCH --ntasks-per-socket=56
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=%TASK_PER_CORE%
#SBATCH --mem=360G
