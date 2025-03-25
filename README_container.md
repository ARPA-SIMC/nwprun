# Use of containers in nwprun

This package makes use of various pieces of software, including
Arpae-SIMC software suite, by means of a
[singularity](https://sylabs.io/singularity/) or
[apptainer](https://apptainer.org/) container, built with the
definition files available in this repository and also distributed in
binary form on public Sylabs repository.

## User documentation

### Available containers

The containers available within this project are:

 * `simc_tools_r8` contains most of the software tools developed at
   Arpae-SIMC (dballe, wreport, libsim, bufr2netcdf, arkimet)

 * `simc_tools_debug_r8` same as the previous but includes also
   debugging tools (gdb, valgrind, strace and debugging symbols)

 * `nwprun_r8` same as `simc_tools_r8` but with some additional python
   libraries used for generating ensemble plots for the Italian
   modelling suites

 * `nwprun_f40` same as `nwprun_r8` but based on a recent Fedora
   distribution, thus with more recent versions of some packages

 * `bufr2netcdf_r8` minimal container with only the bufr2netcdf tool,
   mainly for the [Cosmo](https://www.cosmo-model.org/) community

 * `Singularity.simc_tools_devel_r8` same as `simc_tools_r8` but
   containing also the copilers and development libraries for building
   program with Arpae-SIMC tools.

They are all based on the Rocky8 (compatible with RedHat 8) or on the
Fedora 40 linux distributions, while the extra tools are installed
natively through the Arpae-SIMC [copr
repository](https://copr.fedorainfracloud.org/coprs/simc/stable/)

The containers are published on the sylabs.io repository, thus they
can be downloaded interactively at their [specific
address](https://cloud.sylabs.io/library/dcesari/default/simctools) or
using the `singularity` tool directly:

```
singularity pull library://dcesari/default/simctools:simc_tools_r8
```

### Use of the software within the container

The containers work on any modern Linux distribution. In order to use
them it is necessary to install (as administrator) the singularity or
the apptainer software package. The simplest way to use the tools
included in a container is by executing the container itself (you may
need to previously make it executable with `chmod +x`):

```
./simc_tools_r8.sif vg6d_transform --trans-type=zoom --sub-type=coord \
  --ilon=5. --flon=16. --ilat=40. --flat=48. input.grib output.grib
```

Alternatively you can run an executable through the `singularity exec`
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
`sylabs_deploy.sh` script. In the script the name of the user
repository is hardcoded in the `$SYLABS_REPO` variable, change as
needed.

