# Nwprun

Nwprun combines the configuration and scripting framework
[nwpconf](https://github.com/ARPA-SIMC/nwpconf) with the ECMWF
[ecFlow](https://software.ecmwf.int/wiki/display/ECFLOW/) workflow
manager to create complete suites running Numerical Weather Prediction
models on HPC systems.

It is targeted at the generation and management of operational model
suites contaning the typical tasks involved in continuous and
intermittent atmospheric data assimilation (using various techniques
including ensemble data assimilation), and forecasting (both in
deterministic and in ensemble modes). The main target is real time
suites, but there are options for applying the system to long-period
research and reanalysis suites.

Nwprun includes:
 * a set of job templates for performing the different parts of the
   ecFlow workflow using the nwpconf framework
 * a set of ecFlow include files to be used by the jobs, targeted at
   slurm and pbs schedulers
 * a generic python module for generating ecFlow suites
 * some python suite generators, using the indicated module for
   generating specifical suite definitions
 * a set of configuration trees for a number of NWP suites using the
   nwpconf framework.

The practical configuration files and python suite generators included
in the package are used in the Italian LAMI modelling suites both on
[Cineca](https://www.cineca.it/) and on
[Arpae-SIMC](https://www.arpae.it/sim) HPC systems.
