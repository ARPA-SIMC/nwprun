#!/usr/bin/python3

import os,sys
import datetime
import ecflow
from nwprun import *

common_extra_env = {
    "NNODES_PREMODEL": 4,
    "NNODES_MODEL": 4,
    "NTASKS_PREMODEL": 96,
    "NTASKS_MODEL": 96,
    "NTASKS_POSTPROC": 1,
    "EXTRA_SCHED": "",
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": "maialinux",
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": "",
    "WALL_TIME_WAIT": "04:10:00",
    "WALL_TIME_ARCHIVE": "04:10:00"
}

# suite cosmo_5i_assim
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_5I/assim",
    "WALL_TIME_PREMODEL": "00:20:00",
    "WALL_TIME_MODEL": "01:00:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers, ContinuousAnalysis]}).getconf()
cosmo_5i_assim = ModelSuite("cosmo_5I_assim")
basicenv.add_to(cosmo_5i_assim.suite)
day = cosmo_5i_assim.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=4)).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_5i_assim.check()
cosmo_5i_assim.write()
cosmo_5i_assim.replace()

# suite cosmo_5i_fcast
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_5I/fcast",
    "WALL_TIME_PREMODEL": "00:30:00",
    "WALL_TIME_MODEL": "02:30:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers]}).getconf()
cosmo_5i_fcast = ModelSuite("cosmo_5I_fcast")
basicenv.add_to(cosmo_5i_fcast.suite)
day = cosmo_5i_fcast.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=4)).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_5i_fcast.check()
cosmo_5i_fcast.write()
cosmo_5i_fcast.replace()

# suite 28n_reassim_era
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_28N/reassim_era",
    "WALL_TIME_PREMODEL": "00:20:00",
    "WALL_TIME_MODEL": "01:00:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0", "postproctype": "sync", "cronfreq": 1,
                    "runlist": [EpsMembers, ContinuousAnalysis]}).getconf()
cosmo_28n_reassim_era = ModelSuite("cosmo_28N_reassim_era")
basicenv.add_to(cosmo_28n_reassim_era.suite)
day = cosmo_28n_reassim_era.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 20161201, 20180101))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_28n_reassim_era.check()
cosmo_28n_reassim_era.write()
cosmo_28n_reassim_era.replace()

