
#--------------------------------------------------------------------------------------------------
#
# dEdd_obs
#
#--------------------------------------------------------------------------------------------------

Test Delta Eddington Scheme using observations.

Goal is to compare observed transmittance through sea ice to model prediction
Radiation scheme is DeltaEddington scheme of Briegleb and Light (NCAR, 2007) 
Code was provided through the IcePack sea ice column physical package (thanks to CICE consortium)

The present tesing code was put together by Martin Vancoppenolle and Marion Lebrun — LOCEAN, CNRS, 2020
in the framework of a study on the model representation of light transmittance

# Sources
Sources are in ./SOURCE, the main program file is called "dEdd_obs.F90".
The rest of the source files are from IcePack and belong to the radiation scheme.

# Compilation (./MAKE)
./create_makefile.sh SRC  ---> to create file list
./create_makefile.sh  ---> to create makefile
make ---> to compile "dEdd_obs.x"

You can move the executable dEdd_obs.x in ./RUN to make it comfy


