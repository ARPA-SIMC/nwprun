%include <sched_prolog.h>
#SBATCH --time=%WALL_TIME_MEC%
#SBATCH --nodes=%NNODES_MEC%
%include <%HPCENV%/sched_mpi_common.h>
