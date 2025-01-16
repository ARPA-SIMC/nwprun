%include <sched_prolog.h>
#SBATCH --ntasks=1
#SBATCH --time=%WALL_TIME_WAIT%
%include <%HPCENV%/sched_serial.h>
