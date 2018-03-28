%include <sched_prolog.h>
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
%include <%HPCENV%/sched_serial_load.h>
