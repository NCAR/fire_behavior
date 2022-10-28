#!/bin/bash
#
#################################################
#
# Purpose: Test for a real wildland fire
#
#################################################
#
purge_output=1 # 0) No, 1) yes
#
#################################################
#
test4p1=1 # Check fire area
test4p2=1 # Check heat output
test4p3=1 # Check latent heat output
test4p4=1 # Check Max heat flux
test4p5=1 # Check Max latent heat flux
#
#################################################
#

file_wrf=./test4/rsl.out.0000

file_input=input.txt
file_output=test4_output.txt

cp ./test4/wrf_input.dat .
cp ./test4/namelist.input .
cp ./test4/geo_em.d01.nc .

rm -f ./$file_input ./$file_output
echo '4' > ./$file_input
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

echo "TEST 4:"

if [ $test4p1 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Fire area"
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test4.1 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test4.1 FAILS'
  fi

fi

#
# ----------------------------------------
#

if [ $test4p2 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Heat output"   # 6
  grep "$var" $file_output  | awk '{print $2, $6}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test4.2 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test4.2 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test4p3 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Latent heat output" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test4.3 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test4.3 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test4p4 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max heat flux" # 7
  grep "$var" $file_output  | awk '{print $2, $7}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test4.4 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test4.4 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ $test4p5 -eq 1 ]
then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max latent heat flux" # 8
  grep "$var" $file_output  | awk '{print $2, $8}' > ./file1.dat
  grep "$var" $file_wrf     | awk '{print $2, $8}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ $test -eq 0 ]
  then
    echo '  Test4.5 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test4.5 FAILS'
  fi
fi

#
# ----------------------------------------
#

  # Purge
rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./$file_input ./fort.34 ./wrf_input.dat ./geo_em.d01.nc ./namelist.input
if [ $purge_output -eq 1 ]
then
  rm -rf ./$file_output
fi

  # Print summary of Test 4
if [ $n_test_passed -eq $n_tests ]
then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  exit 1
fi

