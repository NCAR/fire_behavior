#!/bin/bash

MAIN_DIR="$PWD"
TEST_DIR='tests'
TEST1="${TEST_DIR}/test1" 
TEST2="${TEST_DIR}/test2" 
TEST3="${TEST_DIR}/test3" 
#test4="${TEST_DIR}/test4" 


module use ${MAIN_DIR}/modules
module load cheyenne
#module list

#------------------------------------------------------------------------------
# set ESMF_ESMXDIR using ESMFMKFILE
if [ ! -f "${ESMFMKFILE}" ]; then
  echo "ERROR: ESMFMKFILE does not exists."
  exit 1
fi
ESMF_ESMXDIR=`grep "ESMF_ESMXDIR" ${ESMFMKFILE}`
export ESMF_ESMXDIR=${ESMF_ESMXDIR#*=}
#------------------------------------------------------------------------------

echo "Starting tests..."

for mytest in ${TEST2} ${TEST1} ${TEST3}
do 
    echo "------------------------------------------------------------------------------" 
    echo "Running ${mytest}"
    cd ${mytest}
    [[ -f "PET0.ESMF_LogFile" ]] && rm PET0.ESMF_LogFile
    ln -sf ${MAIN_DIR}/build/esmx .
    ln -sf ${MAIN_DIR}/esmxRun.config .
    qcmd -- mpirun -np 2 ./esmx 
    cat PET0.ESMF_LogFile
    cd ${MAIN_DIR}
done    

exit 0
