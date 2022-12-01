#!/bin/bash
#
########################################################
#
url_esmf="https://github.com/esmf-org/esmf"
tag_esmf="v8.3.0"

url_zlib="https://github.com/madler/zlib"
tag_zlib="v1.2.9"

url_hdf5="https://github.com/HDFGroup/hdf5"
tag_hdf5="hdf5-1_13_3"

url_netcdf_c="https://github.com/Unidata/netcdf-c"
tag_netcdf_c="v4.9.0"

url_netcdf_fortran="https://github.com/Unidata/netcdf-fortran"
tag_netcdf_fortran="v4.6.0"

static=1
enable_netcdf4=1
#
########################################################
#
step1=1 # Install ESMF lib
step2=1 # Install NetCDF libs
#
########################################################
#

if [ $step1 -eq 1 ]
then
  rm -rf ./esmf
  git clone -b $tag_esmf $url_esmf
  cd esmf
  export ESMF_DIR=$PWD

  time gmake -j 8

  gmake install

  cd ..
fi

#
# -------------------------------------
#

if [ $step2 -eq 1 ]
then

  netcdf_install_dir=${PWD}/netcdf
  rm -rf $netcdf_install_dir

    # zlib
  rm -rf ./zlib
  git clone -b $tag_zlib $url_zlib 
  cd ./zlib

  if [ $static -eq 1 ]
  then
    flag_static='--static'
  else
    flag_static=''
  fi

  ./configure $flag_static --prefix=$netcdf_install_dir

  time make
  make install

  cd .. 

    # HDF5 lib
  rm -rf ./hdf5
  git clone -b $tag_hdf5 $url_hdf5
  cd ./hdf5

  if [ $static -eq 1 ]
  then
    flag_static='--disable-shared'
  else
     flag_static=''
  fi

  ./configure --prefix=${netcdf_install_dir} --with-zlib=${netcdf_install_dir} --enable-hl $flag_static

  time make -j 8
  make install

  cd ..

    # Netcdf C lib
  rm -rf ./netcdf-c
  git clone -b $tag_netcdf_c $url_netcdf_c
  cd ./netcdf-c

  if [ $static -eq 1 ]
  then
    flag_static='--disable-shared'
  else
    flag_static=''
  fi

  if [ $enable_netcdf4 -eq 1 ]
  then
      # Get access to HDF5 and Z libs
    export LD_LIBRARY_PATH=${netcdf_install_dir}/lib:${LD_LIBRARY_PATH}
    export CPPFLAGS=-I${netcdf_install_dir}/include
    export LDFLAGS=-L${netcdf_install_dir}/lib

    ./configure --enable-netcdf-4 --prefix=${netcdf_install_dir} --disable-dap $flag_static
  else
    ./configure --prefix=${netcdf_install_dir} --disable-dap --disable-netcdf-4  $flag_static
  fi

  time make -j 8
  make install

  cd ..

    # Netcdf Fotran lib
  rm -rf ./netcdf-fortran
  git clone -b $tag_netcdf_fortran $url_netcdf_fortran
  cd ./netcdf-fortran

     # Get access to the C libs
  export LD_LIBRARY_PATH=${netcdf_install_dir}/lib:${LD_LIBRARY_PATH}

    # Set compi flags to see the C lib
  export CPPFLAGS=-I${netcdf_install_dir}/include

  if [ $static -eq 1 ]
  then
    export LIBS=$(${netcdf_install_dir}/bin/nc-config --libs)
    flag_static='--disable-shared'
  else
    export LDFLAGS=-L${netcdf_install_dir}/lib
    flag_static=''
  fi

  ./configure --prefix=${netcdf_install_dir} $flag_static

  time make -j 8
  make install

  cd ..

fi

