%include <sched_prolog.h>
#SBATCH --time=%WALL_TIME_ENDA%
#SBATCH --nodes=%NNODES_ENDA%
%include <%HPCENV%/sched_mpi_common.h>
