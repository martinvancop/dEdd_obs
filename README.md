# dEdd_obs
Test Delta Eddington Scheme using observations

Goal is to compare observed transmittance through sea ice to model prediction

Scheme is from IcePack sea ice column physical package (thanks to CICE consortium)

Reference is Briegleb and Light (NCAR, 2007)

Code put together by Martin Vancoppenolle and Marion Lebrun — LOCEAN, CNRS, 2020

# Compilation
compilation tools are in ./MAKE
./create_makefile.sh SRC to create file list
./create_makefile.sh to create makefile
make to compile "dEdd_obs.x"

you can move the executable in ./RUN to make it comfy

# Sources
Sources are in ./SOURCE, the main program file is called "dEdd_obs.F90".
The rest of the source files are from IcePack and belong to the radiation scheme.


