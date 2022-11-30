#!/bin/bash
#
#################################################
#
# Purpose: Check code reproduces results of test cases
#
#################################################


#################################################
# Functions

testp1to5 () {
    myvar=$1
    mycol=$2
    
    echo "Testing with Arg1=${myvar} Arg2=${mycol}"

    n_tests=$(expr $n_tests + 1)
    [[ -f file1.dat ]] && rm file1.dat
    [[ -f file2.dat ]] && rm file2.dat

    grep "$myvar" $file_output  | awk '{print $2, $"${mycol}"}' > ./file1.dat
    grep "$myvar" $file_wrf     | awk '{print $2, $"${mycol}"}' > ./file2.dat

    test=$(diff ./file1.dat ./file2.dat | wc -l)
    if [ $test -eq 0 ]
    then
	echo "  Test ${myvar} PASSED"
	n_test_passed=$(expr $n_test_passed + 1)
    else
	echo "  Test ${myvar} FAILS"
    fi

}

#################################################
# Main Code
#################################################

# -----------------------------------------------
# Read command line arguments

((!$#)) && echo 'No arguments supplied!' && exit 1

while [ $# -gt 0 ] 
do
    key="${1}"

    case "${key}" in

	# -----------------------------------------------
	# Read test number
	-t=*|--test=*)
	    thistest="${key#*=}"
	    echo "TestAny: running test ${thistest}"
	    shift ## shift past key and value
	    ;;
	-t|--test)
	    thistest="${2}"
	    echo "TestAny: running test ${thistest}"
	    shift 2 ## shift past key and value
	    ;;

	# Optional Arguments:

	# -----------------------------------------------
	# Test using esmx (default is 0, i.e. use standalone)
	--esmx)
	    useesmx=1
	    echo "Using esmx ${useesmx}"
	    shift ## shift past key and value
	    ;;

	# -----------------------------------------------
	# Specify test part (default is 1)
	-p1=*)
	    testp1="${key#*=}"
	    echo "Test part1: ${testp1}"
	    shift ## shift past key and value
	    ;;
	-p2=*)
	    testp2="${key#*=}"
	    echo "Test part2: ${testp2}"
	    shift ## shift past key and value
	    ;;
	-p3=*)
	    testp3="${key#*=}"
	    echo "Test part3: ${testp3}"
	    shift ## shift past key and value
	    ;;
	-p4=*)
	    testp4="${key#*=}"
	    echo "Test part4: ${testp4}"
	    shift ## shift past key and value
	    ;;
	-p5=*)
	    testp5="${key#*=}"
	    echo "Test part5: ${testp5}"
	    shift ## shift past key and value
	    ;;
	# -----------------------------------------------
	-purge=*)
	    purge_output="${key#*=}"
	    echo "Purge test output files: ${purge_output}"
	    shift ## shift past key and value
	    ;;

	*)
    esac
done

thistest="${thistest:?Invalid test argument. Provide arg for test: -t=test1 or -t=1}"

if [[ ${thistest} != test* ]]
then
    if [[ "${#thistest}" -eq 1 ]]
    then
	echo "Setting test name to test${thistest}"
	thistest=test${thistest}
    else
	echo "Something is not right with the argument test"
	exit 1
    fi
fi

#################################################
# defaults

useesmx=${useesmx:=0}
purge_output=${purge_output:=1} # 0) No, 1) yes
testp1=${testp1:=1} # Check fire area
testp2=${testp2:=1} # Check heat output
testp3=${testp3:=1} # Check latent heat output
testp4=${testp4:=1} # Check Max heat flux
testp5=${testp5:=1} # Check Max latent heat flux

#################################################


file_wrf=${thistest}/rsl.out.0000
file_exe=../install/bin/fire_behavior_standalone
file_output=${thistest}_output.txt

# clean up old links/files
[[ -L "wrf_input.dat" ]] && rm wrf_input.dat
[[ -L "namelist.input" ]] && rm namelist.input
[[ -f "${file_output}" ]] && rm -f $file_output

# link files for test
ln -sf ${thistest}/wrf_input.dat .
ln -sf ${thistest}/namelist.input .

#################################################
# Run standalone executable

if [[ ${useesmx} -eq 0 ]]
then
    if [[ -f ${file_exe} ]]
    then
	echo "Running standalone code"
	$file_exe > ./$file_output
    else
	echo 'Please compile the code first'
	exit 1
    fi

#################################################
# Run ESMX

elif  [[ ${useesmx} -eq 1 ]]
then

    if [[ -L esmx ]]
    then
	echo "Running ESMX code"
	qcmd -- mpirun -np 1 ./esmx 
    else
	echo "esmx is not linked."
	exit 1
    fi
fi

#################################################
# Check results

n_tests=0
n_test_passed=0
myvar=""
mycol=""

echo "Results for ${thistest}:"

[[ ${testp1} -eq 1 ]] && var="Fire area"            && col=6 && testp1to5 "${var}" "${col}"
[[ ${testp2} -eq 1 ]] && var="Heat output"          && col=6 && testp1to5 "${var}" "${col}"	      
[[ ${testp3} -eq 1 ]] && var="Latent heat output"   && col=7 && testp1to5 "${var}" "${col}" 
[[ ${testp4} -eq 1 ]] && var="Max heat flux"        && col=7 && testp1to5 "${var}" "${col}"	      
[[ ${testp5} -eq 1 ]] && var="Max latent heat flux" && col=8 && testp1to5 "${var}" "${col}"

#[[ ${thistest} == test1  && ${testp6} -eq 1 ]] && 


#################################################
# Purge

rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./wrf_input.dat ./namelist.input
# don't purge the executable here
# if [[ $purge_output -eq 1 ]]
# then
#   rm -rf ./$file_output
# fi

#################################################
# Print summary of Test 2
if [ $n_test_passed -eq $n_tests ]
then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  exit 1
fi

