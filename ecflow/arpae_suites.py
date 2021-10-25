#!/usr/bin/python3

import os,sys
import datetime
import ecflow
from nwprun import *

common_extra_env = {
    "NNODES_MODEL": 4,
    "EXTRA_SCHED": "",
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": "maialinux",
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": ""
}

# suite cosmo_5i_assim
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_5I/assim"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers, ContinuousAnalysis]}).getconf()
cosmo_5i_assim = ModelSuite("cosmo_5I_assim_ng")
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
    "NWPCONF": "prod/cosmo_5I/fcast"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers]}).getconf()
cosmo_5i_fcast = ModelSuite("cosmo_5I_fcast_ng")
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

# suite cosmo_28n_assim
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_28N/assim"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers, ContinuousAnalysis]}).getconf()
cosmo_28n_assim = ModelSuite("cosmo_28N_assim")
basicenv.add_to(cosmo_28n_assim.suite)
day = cosmo_28n_assim.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int(datetime.datetime.now().strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_28n_assim.check()
cosmo_28n_assim.write()
cosmo_28n_assim.replace()

# suite cosmo_28n_fcruc
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_28N/fcruc"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers]}).getconf()
cosmo_28n_fcruc = ModelSuite("cosmo_28N_fcruc")
basicenv.add_to(cosmo_28n_fcruc.suite)
day = cosmo_28n_fcruc.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int(datetime.datetime.now().strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

cosmo_28n_fcruc.check()
cosmo_28n_fcruc.write()
cosmo_28n_fcruc.replace()

# suite 28n_reassim_era
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_28N/reassim_era"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"],"nwprun"),
                    worktree=os.path.join(os.environ["WORK"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=1,
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

