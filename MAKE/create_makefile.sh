#!/bin/sh
set -xv
SRCDIR="/Users/ioulianikolskaia/Boulot/SCIENCE/RESEARCH/ICE_OPTICS/DELTA_EDDINGTON/SOURCE/dEdd_obs"
archfile=./gfortran_mac.inc
KEY_LIM=""
cppdefs="${KEY_LIM}"

#---------------------------------------------------------------------
# other flags
#---------------------------------------------------------------------
oflags=""
exe=dEdd_obs.x

#---------------------------------------------------------------------
#create source file lists and exit
#---------------------------------------------------------------------
if [ $1 == "SRC" ]; then
   find ${SRCDIR} -name "*.F90" -print > LIST_SRC.lst
exit 0
fi

#---------------------------------------------------------------------
# make makefile                                                                                                    
#---------------------------------------------------------------------
./mkmf -c "${cppdefs}" -o "${oflags}" -t ${archfile} -p ${exe} LIST_SRC.lst
