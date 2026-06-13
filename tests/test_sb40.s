#!/bin/bash
#
#################################################
#
# Purpose: Native Scott and Burgan SB40 ideal-case checks
#
#################################################
#
set -uo pipefail

file_exe=../install/bin/fire_behavior.exe
template=./test9/namelist.fire
work_root="${TMPDIR:-/tmp}/cfbm_sb40_test_$$"

if [ ! -f "$file_exe" ]; then
  echo 'Please compile the code first'
  exit 1
fi

if [ ! -f "$template" ]; then
  echo "TEST SB40:"
  echo "  FAIL: missing template $template"
  exit 1
fi

mkdir -p "$work_root"

write_namelist()
{
  local fuel_cat=$1
  local dest=$2

  awk -v fuel_cat="$fuel_cat" '
    /devel_opt[[:space:]]*=/ {
      print
      print " fuel_opt = 2,"
      next
    }
    /fuel_cat[[:space:]]*=/ {
      print " fuel_cat = " fuel_cat
      next
    }
    { print }
  ' "$template" > "$dest"
}

check_positive_final()
{
  local file_output=$1
  local var=$2

  awk -v var="$var" '
    index($0, var) > 0 { value = $6 + 0.0 }
    END { exit !(value > 0.0) }
  ' "$file_output"
}

run_burnable_case()
{
  local fuel_cat=$1
  local case_dir="$work_root/fuel_${fuel_cat}"
  local file_output="$case_dir/output.txt"

  mkdir -p "$case_dir"
  write_namelist "$fuel_cat" "$case_dir/namelist.fire"

  (cd "$case_dir" && "$OLDPWD/$file_exe" > "$file_output" 2>&1)
  local exe_status=$?
  if [ "$exe_status" -ne 0 ]; then
    echo "  FAIL: fuel_cat=$fuel_cat exited with status $exe_status"
    tail -n 40 "$file_output"
    return 1
  fi

  if ! check_positive_final "$file_output" "Fire area"; then
    echo "  FAIL: fuel_cat=$fuel_cat did not produce positive final fire area"
    tail -n 40 "$file_output"
    return 1
  fi

  if ! check_positive_final "$file_output" "Heat output"; then
    echo "  FAIL: fuel_cat=$fuel_cat did not produce positive final heat output"
    tail -n 40 "$file_output"
    return 1
  fi

  echo "  fuel_cat=$fuel_cat PASSED"
  return 0
}

run_unknown_case()
{
  local fuel_cat=205
  local case_dir="$work_root/fuel_${fuel_cat}"
  local file_output="$case_dir/output.txt"

  mkdir -p "$case_dir"
  write_namelist "$fuel_cat" "$case_dir/namelist.fire"

  (cd "$case_dir" && "$OLDPWD/$file_exe" > "$file_output" 2>&1)
  # Serial Stop_simulation uses Fortran STOP in this codebase, which can
  # return status 0; the diagnostic is the portable rejection signal here.
  if ! grep -q "Unknown nfuel_cat" "$file_output"; then
    echo "  FAIL: fuel_cat=$fuel_cat abort did not report unknown nfuel_cat"
    tail -n 40 "$file_output"
    return 1
  fi

  echo "  fuel_cat=$fuel_cat unknown-code check PASSED"
  return 0
}

echo "TEST SB40:"

n_tests=0
n_test_passed=0

for fuel_cat in 101 106 204; do
  n_tests=$(expr "$n_tests" + 1)
  if run_burnable_case "$fuel_cat"; then
    n_test_passed=$(expr "$n_test_passed" + 1)
  fi
done

n_tests=$(expr "$n_tests" + 1)
if run_unknown_case; then
  n_test_passed=$(expr "$n_test_passed" + 1)
fi

if [ "$n_test_passed" -eq "$n_tests" ]; then
  echo "SUCCESS: $n_test_passed PASSED of $n_tests"
  echo ''
  exit 0
else
  echo "FAILED: $n_test_passed PASSED of $n_tests"
  echo ''
  echo "  Work directory: $work_root"
  exit 1
fi
