#!/usr/bin/python

import os,sys
import datetime
import ecflow
from nwprun import *

common_extra_env = {
    "EXTRA_SCHED": "",
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": "maialinux",
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": "1"
}

# suite moloch_1i_fcast
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/moloch_1I/fcast",
    "NNODES_MODEL": 6,
})

basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env=extra_env)

moloch_1i_fcast = ModelSuite("moloch_1i_fcast")
basicenv.add_to(moloch_1i_fcast.suite)
day = moloch_1i_fcast.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=1)).strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        EpsMembers(membrange="0", modelname="moloch", postprocrange="1-0", wait_obs=False)
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

moloch_1i_fcast.check()
moloch_1i_fcast.write()
moloch_1i_fcast.replace()
