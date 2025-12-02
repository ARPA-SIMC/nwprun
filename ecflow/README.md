## ecFlow use on Cineca HPC cluster

### Source files

Download and unpack:
 * https://software.ecmwf.int/wiki/download/attachments/8650755/boost_1_53_0.tar.gz
 * https://software.ecmwf.int/wiki/download/attachments/8650755/ecFlow-4.6.0-Source.tar.gz

### Build

```
module load gnu
module load qt
module load cmake
cd $WORK/srcgnu
export BOOST_ROOT=$PWD/boost_1_53_0
export WK=$PWD/ecFlow-4.6.0-Source
export PREFIX=$WORK/srcgnu/install

# build boost (no need to install, only static libraries)
cd $BOOST_ROOT
$WK/build_scripts/boost_1_53_fix.sh
./bootstrap.sh
$WK/build_scripts/boost_build.sh

# build ecFlow
cd $WK
mkdir build
cd build
# -DENABLE_SECURE_USER is needed in order to enable authorization by
#  user, not only by port, binary packages distributed by ECMWF do not
#  have this feature
# without UI (Qt not needed)
cmake .. -DCMAKE_INSTALL_PREFIX=$PREFIX -DENABLE_GUI=OFF -DENABLE_UI=OFF -DENABLE_SECURE_USER
# with UI (Qt needed)
cmake .. -DCMAKE_INSTALL_PREFIX=$PREFIX -DENABLE_GUI=OFF -DENABLE_UI=ON -DCMAKE_PREFIX_PATH=$QT_HOME -DENABLE_SECURE_USER
make
make install
```

A line in `ecflow_stop.sh` has to be commented since it has an ECMWF
path hardwired (`/home/ma/emos/bin/ecflow_site.sh`).

### Run

#### Full environment

```
module load gnu
module load qt
export PREFIX=$WORK/srcgnu/install
LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH
PATH=$PREFIX/bin:$PATH
export PYTHONPATH=$PREFIX/lib/python2.7/site-packages
```

After these commands, within the current session, all the ecFlow
executables are in path and the python modules can be imported without
extra operations.

#### Wrapper

Alternatively, a wrapper script has been prepared on Cineca cluster,
in order not to pollute the environment, e.g. in jobs where different
modules may conflict. The wrapper sets the environment on the fly only
for a single command, but it may require update from time to time:

```
$WORKDIR_BASE/nwprun/ecflow/ec_wrap <ecflow_command> <arguments>...
```

### Use

#### Environment

On Cineca cluster, at every login you may land on a different login
node, while ecFlow is sensitive to the host on which the ecFlow server
is run. For this reason, the first login node of the cluster, with
hostname `r000u06l01`, has been declared as the ecFlow main node on
Cineca and the server-related operations should be done by connecting
to `login01.<clustername>.cineca.it`. On the other hand, the client
operations, including running the graphical user interface, can be
performed on any node.

The port number, which must be different for every user willing to use
ecFlow on the same host, is assigned by the `ecflow_start.sh` script
to a default value equal to the Unix user id of the calling user (`id`
command) + 1500. However not all the ecFlow commands respect that
default, so it is a good idea to start once the server with
`ecflow_start.sh`, note down the port number assigned to the user and
set once for all the ecFlow user environment in `.bash_profile`, e.g.:

```
export ECF_HOST=r000u06l01
export ECF_PORT=xxx
export ECF_PASSWD=$HOME/.ecf_passwd
```

The file indicated by `ECF_PASSWD`, which should be readable and
writable only by the user, contains one or more
user/host/port/password entries that prevent other users (not owning
that file) to interact with the server at the user's port. This
feature works only when ecFlow has been built with the
`-DENABLE_SECURE_USER` indicated above.

#### Typical commands

```
# start the server
ecflow_start.sh
# check that the server is running
ecflow_client --ping

# start the graphical user interface
ecflow_ui

# load or replace a .def definition file on the server
ecflow_client --load=<suitefile.def> force
# activate a suite
ecflow_client --begin=<suitename>

# the same in a single command (it may be useful to preliminarly
# suspend the suite through the graphical interface)
ecflow_client --replace=/<suitename> <suitefile.def>

# terminate the server
ecflow_stop.sh
```
