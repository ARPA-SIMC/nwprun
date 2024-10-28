module load profile/global
module load cdo/2.1.0--gcc--11.3.0
module load intel-oneapi-compilers/2023.2.1 
module load intel-oneapi-mkl/2022.2.1 
module load intel-oneapi-mpi/2021.10.0
module load libszip/2.1.1--oneapi--2023.2.0 
module load zlib/1.2.13--gcc--11.3.0
export LD_LIBRARY_PATH=$WORKDIR_BASE/srcintel/eccodes-2.32.0/lib64:$WORKDIR_BASE/srcintel/install/lib:$LD_LIBRARY_PATH
export PATH=$WORKDIR_BASE/srcintel/eccodes-2.32.0/bin:$WORKDIR_BASE/srcintel/install/bin:$PATH
