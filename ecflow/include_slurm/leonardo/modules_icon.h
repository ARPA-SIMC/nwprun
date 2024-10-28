# Enrico
#module load profile/archive
#module load cdo
#module load intel/oneapi-2021--binary
#module load intelmpi/oneapi-2021--binary 
#module load netcdff/4.5.3--oneapi--2021.2.0-ifort 
#module load szip/2.1.1--oneapi--2021.2.0-ifort 
#module load mkl/oneapi-2021--binary
#module load eccodes/2.21.0--intelmpi--oneapi-2021--binary

# 2.6.5.1
module load profile/global 
module load intel-oneapi-compilers/2023.2.1 
module load intel-oneapi-mkl/2022.2.1 
module load intel-oneapi-mpi/2021.10.0
export LD_LIBRARY_PATH=$WORKDIR_BASE/srcintel/eccodes-2.32.0/lib64:$WORKDIR_BASE/srcintel/install/lib:$LD_LIBRARY_PATH
export PATH=$WORKDIR_BASE/srcintel/eccodes-2.32.0/bin:$WORKDIR_BASE/srcintel/install/bin:$PATH
