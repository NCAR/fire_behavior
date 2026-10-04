#!/bin/bash
# Created on 2026-10-02. Developed by the CFBM development team.
# run tests/legacy/run_legacy.sh test7 /path/to/install /path/to/scratch/legacy
# Preserve the original comparisons while isolating their working files.
set -euo pipefail

#--------------------------------------------------------------------------------
# Private legacy layout
#--------------------------------------------------------------------------------
TEST_NAME=$1
INSTALL_ROOT=$2
RUN_ROOT=$3
TEST_SOURCE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
mkdir -p "${RUN_ROOT}"
CASE_ROOT=$(mktemp -d "${RUN_ROOT}/${TEST_NAME}-XXXXXXXX")
mkdir "${CASE_ROOT}/tests"
ln -s "${INSTALL_ROOT}" "${CASE_ROOT}/install"
for FIXTURE in test7 test8 testx; do
  ln -s "${TEST_SOURCE}/${FIXTURE}" "${CASE_ROOT}/tests/${FIXTURE}"
done
cp "${TEST_SOURCE}/esmxRun.yaml" "${CASE_ROOT}/tests/"
cp "${TEST_SOURCE}/esmfRun.config" "${TEST_SOURCE}/fd_fire.yaml" "${CASE_ROOT}/tests/"

#--------------------------------------------------------------------------------
# Preserve diagnostics instead of executing the original cleanup commands
#--------------------------------------------------------------------------------
# The checked-in legacy scripts and numerical comparisons remain unchanged.
# A ':' keeps shell blocks valid when cleanup was their only statement.
while IFS= read -r LINE || [[ -n "${LINE}" ]]; do
  CONTENT="${LINE#"${LINE%%[![:space:]]*}"}"
  case "${CONTENT}" in
    rm\ *) printf ': # Retain legacy diagnostic files\n' ;;
    *) printf '%s\n' "${LINE}" ;;
  esac
done < "${TEST_SOURCE}/${TEST_NAME}.s" > "${CASE_ROOT}/tests/legacy.sh"
echo "Legacy artifacts: ${CASE_ROOT}"
cd "${CASE_ROOT}/tests"
bash legacy.sh
