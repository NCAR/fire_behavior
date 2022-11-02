#!/bin/bash
#
#################################################
#
# Purpose: Check stand-alone code reproduces the initial evolution idealize case
#
#################################################
#
purge_output=1 # 0) No, 1) yes
#
#################################################
#
test1p1=1 # Check fire area
test1p2=1 # Check heat output
test1p3=1 # Check latent heat output
test1p4=1 # Check Max heat flux
test1p5=1 # Check Max latent heat flux
test1p6=1 # Check tendency t,qv first time step
#
#################################################
#

file_wrf=./test1/rsl.out.0000
file_exe=../driver/fire_behavior.exe
file_output=test1_output.txt

cp ./test1/namelist.input .

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
fi

#
# ----------------------------------------
#
if [ $test1p6 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)

  head -n 1 ./fort.34 > ./offline.dat
  head -n 1 ./test1/th_qv_tend.dat > ./online.dat
  test=$(diff ./offline.dat ./online.dat | wc -l)
  rm -f ./offline.dat ./online.dat
  if [ $test -eq 0 ]
  then
    echo '  Test1.6 PASSED (tendencies)'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test1.6 FAILS'
  fi
fi
#
# ----------------------------------------
#

  # Purge
rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./namelist.input
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

