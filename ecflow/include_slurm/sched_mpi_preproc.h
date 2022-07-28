%include <sched_prolog.h>
#SBATCH --time=%WALL_TIME%
#SBATCH --exclusive
%include <%HPCENV%/sched_mpi_preproc.h>
%include <%HPCENV%/sched_mpi_common.h>
