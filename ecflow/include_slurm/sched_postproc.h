%include <sched_prolog.h>
#SBATCH --ntasks=2
#SBATCH --time=%WALL_TIME%
%include <%HPCENV%/sched_serial.h>
