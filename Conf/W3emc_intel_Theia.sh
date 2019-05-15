# *** for Theia (intel) ***
 WORKDIR=/scratch3/NCEPDEV/nwprod/IPS
 MODULEPATH=/apps/lmod/lmod/modulefiles/Core
 module use -a $WORKDIR/modulefiles/core_third
 module load EnvVars/1.0.2
 module load ips/18.0.1.163
 module load impi/2018.0.1
 module load dev/w3emc/2.2.0
 module load dev/sigio/2.1.0
 module load dev/nemsio/2.2.4

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -fPIC"
 export FFLAGS="-O3 -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -fPIC"
 export MPIFFLAGS="-O3 -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS="NEMSIO $NEMSIO_VER, SIGIO $SIGIO_VER"
