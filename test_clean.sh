#!/bin/bash
#
# Delete the selected group(s) of run-time files of "icon_test" suite
#
if [ $# -ne 1 ] ; then
  echo "Delete run-time files of \"icon_test\" suite"
  echo "use: test_clean.sh opt  (opt can be: out, log, wrk, all)"
  exit
fi
if [ $1 = "-h" -o $1 = "--help" ] ; then
  echo "Delete run-time files of \"icon_test\" suite"
  echo "use: test_clean.sh opt  (opt can be: out, log, wrk, all)"
  exit
fi

opt=$1

# Delete outputs
if [ $opt = "out" -o $opt = "all" ] ; then
  for subdir in import_test/configured download_test/configured import_test/sync_lami/ ; do
    echo "### [out] Delete dir ${WORKDIR_BASE}/${subdir}/icon_2I_fcast:\*"
    rm -Rfv ${WORKDIR_BASE}/${subdir}/icon_2I_fcast:*
  done
fi

# Delete log files 
if [ $opt = "log" -o $opt = "all" ] ; then
  echo "### [log] Purge dir ${WORKDIR_BASE}/ecflow/icon_2I_test"
  rm -Rfv ${WORKDIR_BASE}/ecflow/icon_2I_test/*
fi

# Purge working dir
if [ $opt = "wrk" -o $opt = "all" ] ; then
  echo "### [wrk] Purge dir ${WORKDIR_BASE}/test/icon_2I/fcast"
  find ${WORKDIR_BASE}/test/icon_2I/fcast -maxdepth 3 ! -type d -exec rm -v {} \;
fi

exit
