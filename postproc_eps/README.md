## Eps potprocessing ##

This directory contains scripts and other files for postprocessing
of Ensemble Prediction results.

The python scripts have some dependencies and can be run in the
Singularity container [Singularity.nwprun](../Singularity.nwprun)
defined in this project and available on Singularity hub.

[![https://www.singularity-hub.org/static/img/hosted-singularity--hub-%23e32929.svg](https://www.singularity-hub.org/static/img/hosted-singularity--hub-%23e32929.svg)](https://singularity-hub.org/collections/5107)

### Running in Cineca ###

On galileo cluster, assuming that the container is available as
`/gpfs/meteo/lami/nwprun.sif` and the postprocessing tools are in
`/gpfs/meteo/lami/nwprun/postproc_eps`, in order to run a tool the
procedure is:

```
module load singularity # to be done once
singularity exec -B /gpfs/meteo /gpfs/meteo/lami/nwprun.sif \
  /gpfs/meteo/lami/nwprun/postproc_eps/scacchiera_prob.py \
  macroaree_er.shp /gpfs/meteo/lami/prod/cosmo_2I/fcens/fxtr/data \
  /gpfs/meteo/lami/prod/cosmo_2I/fcens/scacchiera
```

the argument `-B /gpfs/meteo` is required for the `/gpfs/meteo`
filesystem to be available when running the container, otherwise only
`$HOME` will be available. Other filesystems can be added in the same
way.
