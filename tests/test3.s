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
test3p1=1 # Check fire area
test3p2=1 # Check heat output
test3p3=1 # Check latent heat output
test3p4=1 # Check Max heat flux
test3p5=1 # Check Max latent heat flux
test3p6=1 # Check tendencies
#
#################################################
#

file_wrf=./test3/rsl.out.0000

file_input=input.txt
file_output=test3_output.txt

cp ./test3/wrf_input.dat .
cp ./test3/namelist.input .

rm -f ./$file_input ./$file_output
echo '3' > ./$file_input
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

echo "TEST 3:"

if [ $test3p1 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Fire area"
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.1 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.1 FAILS'
  fi

fi

#
# ----------------------------------------
#

if [ $test3p2 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Heat output"   # 6
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.2 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.2 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test3p3 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Latent heat output" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.3 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.3 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test3p4 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max heat flux" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.4 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.4 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test3p5 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max latent heat flux" # 8
  grep "$var" $file_output  | awk '{print $2, $8}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $8}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.5 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.5 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test3p6 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)

  test=$(diff ./fort.34 ./test3/th_qv_tend.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test3.6 PASSED (tendencies)'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test3.6 FAILS'
  fi
fi
#
# ----------------------------------------
#

  # Purge
rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./$file_input ./fort.34 ./wrf_input.dat ./namelist.input
if [ $purge_output -eq 1 ]
then
  rm -rf ./$file_output
fi

  # Print summary of Test 3
if [ $n_test_passed -eq $n_tests ]
then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  exit 1
fi

