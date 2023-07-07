# This file is a template for a Linux environment file
# running "source pysource.template.sh" will position all
# the necessary environment variables for telemac
# To adapt to your installation replace word <word> by their local value
###
### TELEMAC settings -----------------------------------------------------------
###
# Path to telemac root dir
export HOMETEL=$HOMETEL
# Adding python scripts to PATH
export PATH=$HOMETEL/scripts/python3:.:$PATH
# Configuration file
export SYSTELCFG=$PWD/systel.reos-ubuntu.cfg
# Name of the configuration to use
export USETELCFG=reos_ubuntu
# Path to this file
export SOURCEFILE=$PWD/pysource.reos.sh
### Python
# To force python to flush its output
export PYTHONUNBUFFERED='true'
### API
export PYTHONPATH=$HOMETEL/scripts/python3:$PYTHONPATH
export LD_LIBRARY_PATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$LD_LIBRARY_PATH
export PYTHONPATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$PYTHONPATH
###
### COMPILERS -----------------------------------------------------------
###
# Here are a few examples for external libraries
#export SYSTEL=/data/projets/projets.002/systel.002

### MPI -----------------------------------------------------------
#export MPIHOME=$SYSTEL/LIBRARY/mpi/ifort.10.1.008
#export PATH=$MPIHOME/bin:$PATH
#export LD_LIBRARY_PATH=$MPIHOME/lib:$LD_LIBRARY_PATH
###
### EXTERNAL LIBRARIES -----------------------------------------------------------
###
### HDF5 -----------------------------------------------------------
#export HDF5HOME=$SYSTEL/LIBRARY/hdf5-1.8.14/arch/C9
#export LD_LIBRARY_PATH=$HDF5HOME/lib:$LD_LIBRARY_PATH
#export LD_RUN_PATH=$HDF5HOME/lib:$MEDHOME/lib:$LD_RUN_PATH
### MED  -----------------------------------------------------------
#export MEDHOME=$SYSTEL/LIBRARY/med-3.2.0/arch/C9
#export LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
#export PATH=$MEDHOME/bin:$PATH
### MUMPS -------------------------------------------------------------
#export MUMPSHOME=$SYSTEL/LIBRARY/mumps/gnu
#export SCALAPACKHOME=$SYSTEL/LIBRARY/scalapack/gnu
#export BLACSHOME=$SYSTEL/LIBRARY/blacs/gnu
### METIS -------------------------------------------------------------
#export METISHOME=$SYSTEL/LIBRARY/metis-5.1.0/arch/C9
#export LD_LIBRARY_PATH=$METISHOME/lib:$LD_LIBRARY_PATH
