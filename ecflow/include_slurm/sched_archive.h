%include <sched_prolog.h>
#SBATCH --ntasks=%NTASKS_POSTPROC%
#SBATCH --time=%WALL_TIME_ARCHIVE%
%include <%HPCENV%/sched_serial.h>
