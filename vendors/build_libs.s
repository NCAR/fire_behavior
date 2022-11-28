#!/bin/bash
#
########################################################
#
url_esmf='https://github.com/esmf-org/esmf'
branch_esmf="release/8.3.0"
url_netcdf_c='https://github.com/Unidata/netcdf-c'
branch_netcdf_c=""
url_zlib='https://github.com/madler/zlib'
#
########################################################
#
step1=0 # Install ESMF lib
step2=1 # Install NetCDF C lib
#
########################################################
#

if [ $step1 -eq 1 ]
then
  rm -rf ./esmf
  git clone $url_esmf
  cd esmf
  git checkout $branch_esmf
  ESMF_DIR=$PWD
  export ESMF_DIR=$ESMF_DIR

  time gmake -j 8

  gmake install
fi

#
# -------------------------------------
#

if [ $step2 -eq 1 ]
then

  rm -rf ./netcdf-c
  git clone $url_netcdf_c

fi
