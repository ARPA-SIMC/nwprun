%include <sched_prolog.h>
#SBATCH --ntasks=1
#SBATCH --time=01:00:00
%include <%HPCENV%/sched_serial_load.h>
