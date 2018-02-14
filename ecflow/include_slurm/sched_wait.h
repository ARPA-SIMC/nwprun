#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
#SBATCH --ntasks=1
#SBATCH --mem=4G
#SBATCH --time=04:10:00
. ~/smnd_profile

