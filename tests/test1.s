#!/bin/bash
#
#################################################
#
# Purpose: Check stand-alone code reproduces the initial evolution of a laminar case with no fire/atm feedbacks
#
#################################################
#
plot_results=0 # 0) No, 1) Yes
purge_output=1 # 0) No, 1) yes
#
#################################################
#
test1p1=1 # Check fire area
test1p2=1 # Check heat output
test1p3=1 # Check latent heat output
test1p4=0 # Check Max heat flux
test1p5=0 # Check Max latent heat flux
#
#################################################
#

file_wrf=./test1/rsl.out.0000

file_input=input.txt
file_output=test1_output.txt

rm -f ./$file_input ./$file_output
echo '1' > ./$file_input
if [ -f ./fire_behavior.exe ]
then
 ./fire_behavior.exe > ./$file_output < ./$file_input
else
  echo 'Please compile the code first'
  exit 1
fi

n_tests=0
n_test_passed=0

#
# ----------------------------------------
#

echo "TEST 1:"

if [ $test1p1 -eq 1 ]
then
    # Test 1.1
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Fire area"
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test1.1 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.1 FAILS'
  fi

  if [ $plot_results -eq 1 ]
  then
    ./test1/gn_dos.s file1.dat file2.dat
    mv ./plot.ps ./test1p1.ps
  fi
fi

#
# ----------------------------------------
#

if [ $test1p2 -eq 1 ]
then
    # Test 1.2
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Heat output"   # 6
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test1.2 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.2 FAILS'
  fi

  if [ $plot_results -eq 1 ]
  then
    ./test1/gn_dos.s file1.dat file2.dat
    mv ./plot.ps ./test1p2.ps
  fi 
fi

#
# ----------------------------------------
#

if [ $test1p3 -eq 1 ]
then
    # Test 1.3
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Latent heat output" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test1.3 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.3 FAILS'
  fi

  if [ $plot_results -eq 1 ]
  then
    ./test1/gn_dos.s file1.dat file2.dat
    mv ./plot.ps ./test1p3.ps
  fi 
fi

#
# ----------------------------------------
#

if [ $test1p4 -eq 1 ]
then

    # Test 1.4
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max heat flux" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test1.4 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.4 FAILS'
  fi

  if [ $plot_results -eq 1 ]
  then
    ./test1/gn_dos.s file1.dat file2.dat
    mv ./plot.ps ./test1p4.ps
  fi 
fi

#
# ----------------------------------------
#

if [ $test1p5 -eq 1 ]
then
    # Test 1.5
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max latent heat flux" # 8
  grep "$var" $file_output  | awk '{print $2, $8}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $8}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test1.5 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.5 FAILS'
  fi

  if [ $plot_results -eq 1 ]
  then
    ./test1/gn_dos.s file1.dat file2.dat
    mv ./plot.ps ./test1p5.ps
  fi 
fi

#
# ----------------------------------------
#

  # Purge
rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./$file_input ./plot.1
if [ $purge_output -eq 1 ]
then
  rm -rf ./$file_output
fi

  # Print summary of Test 1
if [ $n_test_passed -eq $n_tests ]
then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  exit 1
fi

