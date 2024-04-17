#!/bin/bash
set -e
if [ $# -ne 0 ] ; then
  echo "Build a clean and up-to-date icon_test environment"
  echo "All files in icon_test (conf, jobs, running dir, log) are deleted. Files in \"conf\" and \"jobs\" are copied from prod suite"
  exit
fi

echo "All files will be deleted in directories:"
echo ${WORKDIR_BASE}/nwprun/conf/test/icon_2I
echo ${WORKDIR_BASE}/nwprun/ecflow/jobs_test
echo ${WORKDIR_BASE}/import_test
echo ${WORKDIR_BASE}/download_test
echo ${WORKDIR_BASE}/nwprun/ecflow/icon_2I_test/
echo ${WORKDIR_BASE}/test/icon_2I/fcast/
echo "Do you wish to proceed? (Y/N)"
read yn
if [ $yn != "Y" -a $yn != "y" ] ; then
  exit
fi

# Delete source files in "test"
find ${WORKDIR_BASE}/nwprun/conf/test/icon_2I -type f -exec rm {} \;
rm -f ${WORKDIR_BASE}/nwprun/conf/test/icon_2I/fcast/*
rm -f ${WORKDIR_BASE}/nwprun/ecflow/jobs_test/*

# Delete output, log, working dir
for subdir in import_test/configured download_test/configured import_test/sync_lami/ ; do
  rm -Rf ${WORKDIR_BASE}/${subdir}/icon_2I_fcast:*
done
echo rm -Rf ${WORKDIR_BASE}/ecflow/icon_2I_test/*
echo rm -Rf ${WORKDIR_BASE}/test/icon_2I/fcast/*

# If needed, build directories in "test"
mkdir -p ${WORKDIR_BASE}/nwprun/ecflow/jobs_test
mkdir -p ${WORKDIR_BASE}/import_test/sync_lami
mkdir -p ${WORKDIR_BASE}/download_test
mkdir -p ${WORKDIR_BASE}/nwprun/conf/test/icon_2I/fcast

# Copy files from "prod" to "test"
rsync -lptgov ${WORKDIR_BASE}/nwprun/conf/prod/icon_2I/* ${WORKDIR_BASE}/nwprun/conf/test/icon_2I
rsync -lptgov ${WORKDIR_BASE}/nwprun/conf/prod/icon_2I/fcast/* ${WORKDIR_BASE}/nwprun/conf/test/icon_2I/fcast
rsync -lptgov ${WORKDIR_BASE}/nwprun/ecflow/jobs/* ${WORKDIR_BASE}/nwprun/ecflow/jobs_test
cp ${WORKDIR_BASE}/nwprun/conf/test/icon_2I/fcast/conf.sh.test ${WORKDIR_BASE}/nwprun/conf/test/icon_2I/fcast/conf.sh

