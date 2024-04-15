#!/bin/bash
#
# List the files with differences between the suites icon_2I_test and icon_2I  
#
if [ $# -ne 0 ] ; then
  echo "List the files with differences between the suites icon_2I_test and icon_2I"
  exit
fi

echo ""
echo "### list of different files in conf ###"
echo ""
diff -qr ${WORKDIR_BASE}/nwprun/conf/prod/icon_2I ${WORKDIR_BASE}/nwprun/conf/test/icon_2I | grep -Ev "enda|enda_dia|fcens|fcruc"
echo ""
echo "### list of different files in jobs ###"
echo ""
diff -qr ${WORKDIR_BASE}/nwprun/ecflow/jobs ${WORKDIR_BASE}/nwprun/ecflow/jobs_test
