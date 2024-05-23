# Use of containers in nwprun

This package makes use of various pieces of software, including
Arpae-SIMC software suite, by means of a
[singularity](https://sylabs.io/singularity/) or
[apptainer](https://apptainer.org/) container built with the
definition files available in this repository and also distributed in
binary form on Sylabs repository.

## User documentation

### Available containers

The containers available within this project are:

 * `simc_tools_r8` contains most of the software tools developed at
   Arpae-SIMC (dballe, wreport, libsim, bufr2netcdf, arkimet)

 * `simc_tools_debug_r8` same as the previous but includes also
   debugging tools (gdb, valgrind, strace and debugging symbols)

 * `nwprun_r8` same as `simc_tools_r8` but with some additional python
   libraries used for generating ensemble plots for the LAMI modelling
   suites

 * `bufr2netcdf_r8` minimal container with only the bufr2netcdf tool,
   mainly for the [Cosmo](https://www.cosmo-model.org/) community

They are all based on the Rocky8 linux distribution (analogous to
RedHat 8) and the extra tools are installed through the Arpae-SIMC
[copr
repository](https://copr.fedorainfracloud.org/coprs/simc/stable/)

The containers are published on the sylabs.io repository so they can
be downloaded interactively at their [specific
address](https://cloud.sylabs.io/library/dcesari/default/simctools) or
using the `singularity` tool directly:

```
singularity pull library://dcesari/default/simctools:simc_tools_r8
```

### Use of the software within the container

In order to use the containers it is necessary to install (as
administrator) the singularity or the apptainer software. The simplest
way to use the tools in the container is by executing the container
itself (you may need to make it executable with `chmod +x`):

```
./simc_tools_r8.sif vg6d_transform --trans-type=zoom --sub-type=coord \
  --ilon=5. --flon=16. --ilat=40. --flat=48. input.grib output.grib
```

Alternatively you can run an executable through the singularity
command (e.g. in case you need to add command-line arguments to
singularity):

```
singularity exec ./simc_tools_r8.sif vg6d_transform --trans-type=zoom --sub-type=coord \
  --ilon=5. --flon=16. --ilat=40. --flat=48. input.grib output.grib
```

A frequently needed singularity argument is the `-B <directory>`
(possibly repeated multiple times) which allows to "see" in the
container a mount point other than the one containing the user's home
directory.

You can also open a shell in the container and explore it
interactively with the command:

```
singularity shell ./simc_tools_r8.sif
```

## Developer documentation

### Building and publishing containers

Execute `singularity remote login` and, in case of failure, get a new
access token interactively by following the instructions, then run the
`sylabs_deploy.sh` script.

