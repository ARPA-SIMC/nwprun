%include <sched_prolog.h>
#SBATCH --time=%WALL_TIME%
#SBATCH --exclusive
%include <%HPCENV%/sched_mpi_model.h>
%include <%HPCENV%/sched_mpi_common.h>
