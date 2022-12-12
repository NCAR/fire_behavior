#!/bin/bash
#
#################################################
#
# Purpose: Check stand-alone code reproduces the initial evolution of a laminar case with no fire/atm feedbacks
#
#################################################
#
purge_output=1 # 0) No, 1) yes
#
#################################################
#
test2p1=1 # Check fire area
test2p2=1 # Check heat output
test2p3=1 # Check latent heat output
test2p4=1 # Check Max heat flux
test2p5=1 # Check Max latent heat flux
#
#################################################
#

file_wrf=./test2/rsl.out.0000
file_exe=../driver/fire_behavior.exe
file_output=test2_output.txt

cp ./test2/wrf_input.dat .
cp ./test2/namelist.input .

rm -f ./$file_output
if [ -f $file_exe ]
then
  $file_exe > ./$file_output
else
  echo 'Please compile the code first'
  exit 1
fi

n_tests=0
n_test_passed=0

#
# ----------------------------------------
#

echo "TEST 2:"

if [ $test2p1 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Fire area"
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test2.1 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test2.1 FAILS'
  fi

fi

#
# ----------------------------------------
#

if [ $test2p2 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Heat output"   # 6
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test2.2 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test2.2 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test2p3 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Latent heat output" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test2.3 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test2.3 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test2p4 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max heat flux" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test2.4 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test2.4 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test2p5 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max latent heat flux" # 8
  grep "$var" $file_output  | awk '{print $2, $8}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $8}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test2.5 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test2.5 FAILS'
  fi
fi

#
# ----------------------------------------
#

  # Purge
rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./wrf_input.dat ./namelist.input
if [ $purge_output -eq 1 ]
then
  rm -rf ./$file_output
  rm -rf ./fire_output_2000-01-01_00:00:02.nc
fi

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

