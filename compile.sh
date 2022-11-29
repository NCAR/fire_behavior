#!/bin/bash 

# usage instructions
usage () {
  printf "Usage: $0 [OPTIONS]...\n"
  printf "\n"
  printf "OPTIONS\n"
  printf "  --system=SYSTEM\n"
  printf "      name of machine (e.g. 'cheyenne')\n"
  printf "  --module-dir=MODULE_DIR\n"
  printf "      path to modulefiles\n"
  printf "  --module-file=MODULE_FILE\n"
  printf "      module file\n"
  printf "  --build-dir=BUILD_DIR\n"
  printf "      build directory\n"
  printf "  --build-type=BUILD_TYPE\n"
  printf "      build type; valid options are 'debug', 'release',\n"
  printf "      'relWithDebInfo'\n"
  printf "  --install-dir=INSTALL_DIR\n"
  printf "      installation prefix\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  MODULE_DIR=${MODULE_DIR}\n"
  printf "  MODULE_FILE=${MODULE_FILE}\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "  BUILD_TYPE=${BUILD_TYPE}\n"
  printf "  INSTALL_DIR=${INSTALL_DIR}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# find system name
find_system () {
    local sysname=`hostname`
    sysname="${sysname//[[:digit:]]/}"
    echo "$sysname"
}

#------------------------------------------------------------------------------

# default settings
FIRE_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
SYSTEM=""
MODULE_DIR="${FIRE_DIR}/modules"
MODULE_FILE=""
BUILD_DIR="${FIRE_DIR}/ufs_fire_build"
BUILD_TYPE="release"
INSTALL_DIR="${FIRE_DIR}/install"
VERBOSE=false

#------------------------------------------------------------------------------

# required arguments
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  exit 0
fi

# process arguments
while :; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --system=?*) SYSTEM=${1#*=} ;;
    --system) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --system=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --module-dir=?*) MODULE_DIR=${1#*=} ;;
    --module-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --module-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --module-file=?*) MODULE_FILE=${1#*=} ;;
    --module-file) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --module-file=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-dir=?*) BUILD_DIR=${1#*=} ;;
    --build-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-type=?*) BUILD_TYPE=${1#*=} ;;
    --build-type) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-type=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --install-dir=?*) INSTALL_DIR=${1#*=} ;;
    --install-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --install-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --verbose|-v) VERBOSE=true ;;
    --verbose=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) break
  esac
  shift
done

set -eu
#------------------------------------------------------------------------------

# automatically determine system
if [ -z "${SYSTEM}" ] ; then
  SYSTEM=$(find_system)
fi

# automatically determine module file
if [ -z "${MODULE_FILE}" ] ; then
  MODULE_FILE="${SYSTEM}"
fi

# print settings
if [ "${VERBOSE}" = true ] ; then
  settings
fi

# load environment using modulefile
if [ ! -d "${MODULE_DIR}/${MODULE_FILE}" ]; then
  printf "ERROR: ${MODULE_FILE} does not exist in ${MODULE_DIR}.\n"
  printf "\n"
  exit 1
fi
module use ${MODULE_DIR}
module load ${MODULE_FILE}
module list

#------------------------------------------------------------------------------
# set ESMF_ESMXDIR using ESMFMKFILE
if [ ! -f "${ESMFMKFILE}" ]; then
  echo "ERROR: ESMFMKFILE does not exists."
  exit 1
fi
ESMF_ESMXDIR=`grep "ESMF_ESMXDIR" ${ESMFMKFILE}`
export ESMF_ESMXDIR=${ESMF_ESMXDIR#*=}

#------------------------------------------------------------------------------
mkdir -p ${BUILD_DIR}
mkdir -p ${FIRE_DIR}/build

# cmake settings
CMAKE_SETTINGS="-DCMAKE_BUILD_TYPE=${BUILD_TYPE}"
#CMAKE_SETTINGS="${CMAKE_SETTINGS} -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}"

# make settings
MAKE_SETTINGS=""
if [ "${VERBOSE}" = true ]; then
  MAKE_SETTINGS="VERBOSE=1"
fi

# build the code
# cd ${BUILD_DIR}
# cmake ${FIRE_DIR} ${CMAKE_SETTINGS}
# make -j ${BUILD_JOBS:-4} ${MAKE_SETTINGS}
# make install

#------------------------------------------------------------------------------
# remove link
[[ -l "${FIRE_DIR}/esmx" ]] && rm ${FIRE_DIR}/esmx

echo "Using python: $(which python)"

# build and install MyModel
cmake -S${FIRE_DIR} -B${BUILD_DIR} \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
  -DCMAKE_MODULE_PATH="${ESMF_ESMXDIR}/Driver/cmake" 
cmake --build ${BUILD_DIR} -v
cmake --install ${BUILD_DIR}

echo "working on patch..."
# patch mymodel.cmake for esmx_driver
# to be moved to ESMX build system
echo "target_link_libraries(esmx_driver PUBLIC fire_behavior_nuopc)" >> "${INSTALL_DIR}"/cmake/fire_behavior_nuopc.cmake

# build and install application
cmake -S${ESMF_ESMXDIR} -Bbuild \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
  -DCMAKE_PREFIX_PATH=${INSTALL_DIR} 

cmake --build ./build -v
cmake --install ./build

if [[ $? -ne 0 ]]
then
    echo "Error"
    exit 1
else 
    ln -sf build/esmx .
fi
