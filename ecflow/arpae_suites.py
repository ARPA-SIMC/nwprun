#!/usr/bin/python

import os,sys
import datetime
import ecflow
from nwprun import *

# suite cosmo_5i_assim
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/cosmo_5I/assim",
                        "NNODES_MODEL": 4,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

cosmo_5i_assim = ModelSuite("cosmo_5I_assim")
basicenv.add_to(cosmo_5i_assim.suite)
day = cosmo_5i_assim.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=1)).strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        GetObs(gts=True, lhn=True),
        EpsMembers(membrange="0", postprocrange="0", wait_obs=True),
        ContinuousAnalysis()
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_5i_assim.check()
cosmo_5i_assim.write()
cosmo_5i_assim.replace()

# suite cosmo_5i_fcast
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/cosmo_5I/fcast",
                        "NNODES_MODEL": 4,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

cosmo_5i_fcast = ModelSuite("cosmo_5I_fcast")
basicenv.add_to(cosmo_5i_fcast.suite)
day = cosmo_5i_fcast.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=1)).strftime("%Y%m%d")),
                      20201228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        GetObs(gts=True, lhn=True),
        EpsMembers(membrange="0", postprocrange="0", wait_obs=True)
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_5i_fcast.check()
cosmo_5i_fcast.write()
cosmo_5i_fcast.replace()

# suite cosmo_28n_assim
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/cosmo_28N/assim",
                        "NNODES_MODEL": 4,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

cosmo_28n_assim = ModelSuite("cosmo_28N_assim")
basicenv.add_to(cosmo_28n_assim.suite)
day = cosmo_28n_assim.suite.add_family("day").add_repeat(
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
        EpsMembers(membrange="0", postprocrange="0", wait_obs=True),
        ContinuousAnalysis()
    ]
    ).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_28n_assim.check()
cosmo_28n_assim.write()
cosmo_28n_assim.replace()

# suite cosmo_28n_fcruc
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/cosmo_28N/fcruc",
                        "NNODES_MODEL": 4,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

cosmo_28n_fcruc = ModelSuite("cosmo_28N_fcruc")
basicenv.add_to(cosmo_28n_fcruc.suite)
day = cosmo_28n_fcruc.suite.add_family("day").add_repeat(
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

cosmo_28n_fcruc.check()
cosmo_28n_fcruc.write()
cosmo_28n_fcruc.replace()

# suite 28n_reassim_era
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/cosmo_28N/reassim_era",
                        "NNODES_MODEL": 4,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

cosmo_28n_reassim_era = ModelSuite("cosmo_28N_reassim_era")
basicenv.add_to(cosmo_28n_reassim_era.suite)
day = cosmo_28n_reassim_era.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 20161201, 20180101))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, runlist=[
        EpsMembers(membrange="0", postprocrange="0", postproctype="sync", wait_obs=False),
        ContinuousAnalysis()
    ], cronfreq=1).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_28n_reassim_era.check()
cosmo_28n_reassim_era.write()
cosmo_28n_reassim_era.replace()

# suite moloch_1i_fcast
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env={
                        "NWPCONF": "prod/moloch_1I/fcast",
                        "NNODES_MODEL": 6,
                        "EXTRA_SCHED": "",
                        "NO_FAIL": "FALSE",
                        "TASK_PER_CORE": "1",
                        "HPCENV": "maialinux"
                    })

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
