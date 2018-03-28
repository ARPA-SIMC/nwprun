%include <sched_prolog.h>
#SBATCH --ntasks=1
#SBATCH --time=%WALL_TIME%
%include <%HPCENV%/sched_serial.h>
