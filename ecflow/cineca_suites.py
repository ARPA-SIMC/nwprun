#!/usr/bin/python

import os,sys
import datetime
import ecflow
from nwprun import *

# dirty trick to move suites from scratch to meteo
os.environ["CINECA_SCRATCH"] = "/marconi_meteo/lami"

common_extra_env = {
    "EXTRA_SCHED": "--partition=bdw_meteo_prod --qos=bdw_qos_meteoenda -A smr_prod",
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": "marconi",
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": "1"
}

# Suite enda
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/enda",
    "NNODES_MODEL": 2,
    "NNODES_ENDA": 4
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["CINECA_SCRATCH"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=1,
                    extra_env=extra_env)

enda = ModelSuite("cosmo_2I_enda")
basicenv.add_to(enda.suite)
day = enda.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int(datetime.datetime.now().strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        GetObs(gts=True, lhn=True),
        EpsMembers(membrange="0-20", postprocrange="0", wait_obs=True),
        EndaAnalysis()
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

enda.check()
enda.write()
enda.replace()

# Suite fcruc
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/fcruc",
    "NNODES_MODEL": 8,
    "NNODES_ENDA": 4
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["CINECA_SCRATCH"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=1,
                    extra_env=extra_env)

fcruc = ModelSuite("cosmo_2I_fcruc")
basicenv.add_to(fcruc.suite)
day = fcruc.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int(datetime.datetime.now().strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        GetObs(gts=True, lhn=True),
        EpsMembers(membrange="0", postprocrange="0", wait_obs=True)
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

fcruc.check()
fcruc.write()
fcruc.replace()

# Suite fcens
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/fcens",
    "NNODES_MODEL": 6,
    "NNODES_ENDA": 4,
    "EXTRA_SCHED": "--partition=bdw_meteo_prod --qos=bdw_qos_meteoeps -A smr_prod",
    "ECF_TIMEOUT": "14400"
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["CINECA_SCRATCH"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=1,
                    extra_env=extra_env)

fcens = ModelSuite("cosmo_2I_fcens")
basicenv.add_to(fcens.suite)
day = fcens.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=1)).strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(21, 24, 3): # h=21
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        GetObs(gts=True, lhn=True),
        EpsMembers(membrange="1-20", postprocrange="1-20", wait_obs=True),
        EpsPostproc()
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

fcens.check()
fcens.write()
fcens.replace()

