#!/bin/bash
#
#################################################
#
# Purpose: Regression for the idealized test9 fire spread case
#
#################################################
#
set -uo pipefail

purge_output=${purge_output:-1} # 0) No, 1) yes
update_baseline=${UPDATE_BASELINE:-0} # 0) Compare, 1) write test9/test_solution.txt
launcher=${FIRE_TEST_LAUNCHER:-}
#
#################################################
#
test9p1=1 # Check fire area
test9p2=1 # Check heat output
test9p3=1 # Check latent heat output
test9p4=1 # Check Max heat flux
test9p5=1 # Check Max latent heat flux
#
#################################################
#

file_baseline=./test9/test_solution.txt
file_exe=../install/bin/fire_behavior.exe
file_output=test9_output.txt

cleanup()
{
  rm -f ./namelist.fire.output ./file1.dat ./file2.dat ./namelist.fire
  if [ "$purge_output" -eq 1 ]; then
    rm -f ./"$file_output"
    rm -f ./fire_output_2020-01-01_00:??:??.nc
  fi
}
trap cleanup EXIT

if [ ! -f "$file_exe" ]; then
  echo 'Please compile the code first'
  exit 1
fi

cp ./test9/namelist.fire ./namelist.fire
rm -f ./"$file_output"

if [ -n "$launcher" ]; then
  read -r -a launch_cmd <<< "$launcher"
  "${launch_cmd[@]}" "$file_exe" > "$file_output" 2>&1
else
  "$file_exe" > "$file_output" 2>&1
fi
exe_status=$?

if [ "$exe_status" -ne 0 ]; then
  echo "TEST 9:"
  echo "  FAIL: fire_behavior.exe failed"
  tail -n 40 "$file_output"
  exit 1
fi

if [ "$update_baseline" -eq 1 ]; then
  cp "$file_output" "$file_baseline"
  echo "  Wrote baseline: $file_baseline"
  exit 0
fi

if [ ! -f "$file_baseline" ]; then
  echo "TEST 9:"
  echo "  FAIL: missing baseline $file_baseline"
  echo "        Run from tests/ with UPDATE_BASELINE=1 ./test9.s after validating the output."
  exit 1
fi

n_tests=0
n_test_passed=0
pass=false

echo "TEST 9:"

#
# ----------------------------------------
#

if [ "$test9p1" -eq 1 ]; then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Fire area"
  grep "$var" "$file_output"   | awk '{print $2, $6}' > ./file1.dat
  grep "$var" "$file_baseline" | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ "$test" -eq 0 ]; then
    echo '  Test9.1 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test9.1 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ "$test9p2" -eq 1 ]; then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Heat output"
  grep "$var" "$file_output"   | awk '{print $2, $6}' > ./file1.dat
  grep "$var" "$file_baseline" | awk '{print $2, $6}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ "$test" -eq 0 ]; then
    echo '  Test9.2 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test9.2 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ "$test9p3" -eq 1 ]; then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Latent heat output"
  grep "$var" "$file_output"   | awk '{print $2, $7}' > ./file1.dat
  grep "$var" "$file_baseline" | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ "$test" -eq 0 ]; then
    echo '  Test9.3 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test9.3 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ "$test9p4" -eq 1 ]; then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max heat flux"
  grep "$var" "$file_output"   | awk '{print $2, $7}' > ./file1.dat
  grep "$var" "$file_baseline" | awk '{print $2, $7}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ "$test" -eq 0 ]; then
    echo '  Test9.4 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test9.4 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ "$test9p5" -eq 1 ]; then
  n_tests=$(expr $n_tests + 1)
  rm -f ./file1.dat ./file2.dat
  var="Max latent heat flux"
  grep "$var" "$file_output"   | awk '{print $2, $8}' > ./file1.dat
  grep "$var" "$file_baseline" | awk '{print $2, $8}' > ./file2.dat

  test=$(diff ./file1.dat ./file2.dat | wc -l)
  if [ "$test" -eq 0 ]; then
    echo '  Test9.5 PASSED'
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo '  Test9.5 FAILS'
  fi
fi

#
# ----------------------------------------
#

if [ "$n_test_passed" -eq "$n_tests" ]; then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
  pass=true
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  pass=false
fi

if [ "$pass" = true ]; then
  exit 0
else
  exit 1
fi
