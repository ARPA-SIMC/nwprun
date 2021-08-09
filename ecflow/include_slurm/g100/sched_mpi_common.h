#SBATCH --partition=g100_meteo_prod
#SBATCH --ntasks-per-node=46
#SBATCH --sockets-per-node=2
##SBATCH --cores-per-socket=23
#SBATCH --ntasks-per-socket=23
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=%TASK_PER_CORE%
#SBATCH --mem=360G
