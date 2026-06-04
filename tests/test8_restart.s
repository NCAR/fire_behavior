#!/bin/bash
#
#################################################
#
# Purpose: Restart regression for the real test8 fire spread case with FMC.
#
#################################################
#
set -uo pipefail

purge_output=${purge_output:-1} # 0) No, 1) yes
launcher=${FIRE_TEST_LAUNCHER:-}

repo_dir=$(cd .. && pwd)
test_dir=$(pwd)
file_exe="$repo_dir/install/bin/fire_behavior.exe"
file_base_namelist="$test_dir/test8/namelist.fire"
file_wrf="$test_dir/test8/wrf.nc"
file_geo="$test_dir/test8/geo_em.d01.nc"
work_dir="$test_dir/restart_test8"

continuous_dir="$work_dir/continuous"
restart_write_dir="$work_dir/restart_write"
restart_read_dir="$work_dir/restart_read"

continuous_output="$continuous_dir/test8_continuous_output.txt"
restart_read_output="$restart_read_dir/test8_restart_read_output.txt"
file_restart_5="$restart_write_dir/fire_restart_2012-06-25_18:00:05.nc"

cleanup()
{
  rm -f "$test_dir"/file1.dat "$test_dir"/file2.dat "$test_dir"/restart_diff.dat
  if [ "$purge_output" -eq 1 ]; then
    rm -rf "$work_dir"
  fi
}
trap cleanup EXIT

if [ ! -f "$file_exe" ]; then
  echo 'Please compile the code first'
  exit 1
fi

if [ ! -f "$file_base_namelist" ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: missing base namelist $file_base_namelist"
  exit 1
fi

if [ ! -f "$file_wrf" ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: missing WRF input $file_wrf"
  exit 1
fi

if [ ! -f "$file_geo" ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: missing geogrid input $file_geo"
  exit 1
fi

rm -rf "$work_dir"
mkdir -p "$continuous_dir" "$restart_write_dir" "$restart_read_dir"

if [ -n "$launcher" ]; then
  read -r -a launch_cmd <<< "$launcher"
fi

configure_namelist()
{
  local file_namelist=$1
  local start_second=$2
  local end_second=$3
  local restart_value=$4
  local restart_interval=$5

  perl -0pi -e 's/^\s*restart\s*=\s*\.(true|false)\.\s*,?\n//img; s/^\s*restart_interval\s*=\s*[-+0-9]+\s*,?\n//img' "$file_namelist"
  perl -0pi -e "s/start_second\\s*=\\s*[-+0-9]+/start_second = $start_second/; s/end_second\\s*=\\s*[-+0-9]+/end_second = $end_second/" "$file_namelist"
  perl -0pi -e "s/(^\\s*\\/\\s*\\n)/  restart = .$restart_value.\\n  restart_interval = $restart_interval\\n\$1/m" "$file_namelist"
}

copy_inputs()
{
  local run_dir=$1
  local copy_geo=$2

  cp "$file_base_namelist" "$run_dir/namelist.fire"
  cp "$file_wrf" "$run_dir/wrf.nc"
  if [ "$copy_geo" = true ]; then
    cp "$file_geo" "$run_dir/geo_em.d01.nc"
  fi
}

run_model()
{
  local run_dir=$1
  local file_output=$2

  if [ -n "$launcher" ]; then
    (cd "$run_dir" && "${launch_cmd[@]}" "$file_exe" > "$file_output" 2>&1)
  else
    (cd "$run_dir" && "$file_exe" > "$file_output" 2>&1)
  fi
}

compare_metric()
{
  local label=$1
  local metric=$2
  local value_column=$3

  rm -f "$test_dir"/file1.dat "$test_dir"/file2.dat "$test_dir"/restart_diff.dat

  grep "$metric" "$continuous_output" | awk -v col="$value_column" '$2 >= 5.5 {print $2, $col}' > "$test_dir"/file1.dat
  grep "$metric" "$restart_read_output" | awk -v col="$value_column" '{print $2, $col}' > "$test_dir"/file2.dat

  if diff -u "$test_dir"/file1.dat "$test_dir"/file2.dat > "$test_dir"/restart_diff.dat; then
    echo "  $label PASSED"
    n_test_passed=$(expr $n_test_passed + 1)
  else
    echo "  $label FAILS"
    if [ -s "$test_dir"/restart_diff.dat ]; then
      cat "$test_dir"/restart_diff.dat
    fi
  fi
  n_tests=$(expr $n_tests + 1)
}

copy_inputs "$continuous_dir" true
configure_namelist "$continuous_dir/namelist.fire" 0 10 false -1

copy_inputs "$restart_write_dir" true
configure_namelist "$restart_write_dir/namelist.fire" 0 5 false 5

copy_inputs "$restart_read_dir" false
configure_namelist "$restart_read_dir/namelist.fire" 5 10 true -1

run_model "$continuous_dir" "$continuous_output"
exe_status=$?
if [ "$exe_status" -ne 0 ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: continuous run failed"
  tail -n 40 "$continuous_output"
  exit 1
fi

run_model "$restart_write_dir" "$restart_write_dir/test8_restart_write_output.txt"
exe_status=$?
if [ "$exe_status" -ne 0 ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: restart write run failed"
  tail -n 40 "$restart_write_dir/test8_restart_write_output.txt"
  exit 1
fi

if [ ! -f "$file_restart_5" ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: missing restart file $file_restart_5"
  exit 1
fi

cp "$file_restart_5" "$restart_read_dir/"

run_model "$restart_read_dir" "$restart_read_output"
exe_status=$?
if [ "$exe_status" -ne 0 ]; then
  echo "TEST 8 RESTART:"
  echo "  FAIL: restart read run failed"
  tail -n 40 "$restart_read_output"
  exit 1
fi

n_tests=0
n_test_passed=0
pass=false

echo "TEST 8 RESTART:"

compare_metric 'Test8_restart.1' 'Fire area' 6
compare_metric 'Test8_restart.2' 'Heat output' 6
compare_metric 'Test8_restart.3' 'Latent heat output' 7
compare_metric 'Test8_restart.4' 'Max heat flux' 7
compare_metric 'Test8_restart.5' 'Max latent heat flux' 8

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
