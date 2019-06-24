#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
%include <%HPCENV%/sched_accounting.h>
