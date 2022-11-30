#!/bin/bash

TEST_DIR="$PWD"
MAIN_DIR="${TEST_DIR}/../"

TEST1="test1" 
TEST2="test2" 
TEST3="test3" 
#test4="${TEST_DIR}/test4" 

echo "Test dir: ${TEST_DIR}"
echo "Main dir: ${MAIN_DIR}"

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

for mytest in ${TEST2} #${TEST1} ${TEST3}
do 
    echo "------------------------------------------------------------------------------" 
    echo "Running ${mytest}"
    cd ${TEST_DIR}/${mytest}

    # clean old log files
    for oldfile in PET*.ESMF_LogFile 
    do
	[[ -f "${oldfile}" ]] && rm ${oldfile}
    done
    # clean old links
    [[ -L "esmx" ]] && rm esmx
    [[ -L "esmxRun.config" ]] && rm esmxRun.config

    # check executable is present
    if [[ ! -f "${MAIN_DIR}/build/esmx" ]] 
    then
	echo "Executable build/esmx is not present. Is it compiled?"
	exit 1
    fi

    ln -sf ${MAIN_DIR}/build/esmx .
    ln -sf ${TEST_DIR}/esmxRun.config .
    
    # qcmd -- mpirun -np 1 ./esmx 
    ./testany.sh -t=${mytest} --esmx
    cat PET0.ESMF_LogFile
    cd ${TEST_DIR}

done    

exit 0
