## Eps potprocessing ##

This directory contains scripts and other files for postprocessing
of Ensemble Prediction results.

The python scripts have some dependencies and can be run in the
Singularity container [Singularity.nwprun](../Singularity.nwprun)
defined in this project and soon available on Singularity hub.

### Running in Cineca ###

On galileo cluster, assuming that the container is available as
`/gpfs/meteo/lami/nwprun.sif` and the postprocessing tools are in
`/gpfs/meteo/lami/nwprun/postproc_eps`, in order to run a tool the
procedure is:

```
module load singularity # to be done once
singularity exec -B /gpfs/meteo /gpfs/meteo/lami/nwprun.sif \
  /gpfs/meteo/lami/nwprun/postproc_eps/scacchiera_prob.py [arg1 [arg2...]]
```

the argument `-B /gpfs/meteo` is required for the `/gpfs/meteo`
filesystem to be available when running hte container, otherwise only
`$HOME` will be available. Other filesystems can be added in this way.