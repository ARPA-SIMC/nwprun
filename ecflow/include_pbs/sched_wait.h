#!/bin/bash
#PBS -o %ECF_JOBOUT%
#PBS -j oe
#PBS -l select=1:ncpus=1:mpiprocs=1:mem=4gb
#PBS -l place=pack:shared
#PBS -l walltime=03:00:00
. ~/smnd_profile

