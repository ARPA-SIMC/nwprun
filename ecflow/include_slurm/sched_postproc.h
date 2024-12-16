%include <sched_prolog.h>
#SBATCH --ntasks=%NTASKS_POSTPROC%
#SBATCH --time=%WALL_TIME%
%include <%HPCENV%/sched_serial.h>
