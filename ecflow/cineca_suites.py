#!/usr/bin/python

import os,sys
import datetime
import optparse
import ecflow
from nwprun import *

parser = optparse.OptionParser(usage="%prog [OPTIONS]")
parser.add_option("--yes", help="work in non-interactive mode and answer yes to all questions (it will overwrite files and replace scripts on server)",
                  action="store_true")
parser.add_option("--delta", help="comma-separated list of delta time in days to go back, for each suite",
                  default="0,0,0")

opts, args = parser.parse_args()
interactive = not opts.yes
delta = [int(i) for i in opts.delta.split(',')]

common_extra_env = {
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": os.environ["HPC_SYSTEM"],
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": ""
}

# Suite enda
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/enda",
    "NNODES_MODEL": 3,
    "NNODES_ENDA": 4
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "membrange": "0-40",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers, EndaAnalysis],
                    "preproc_wt":"00:20:00", "model_wt": "01:00:00"}).getconf()
enda = ModelSuite("cosmo_2I_enda")
basicenv.add_to(enda.suite)
day = enda.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[0])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

enda.check()
enda.write(interactive=interactive)
enda.replace(interactive=interactive)


# Suite enda_radvol
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/enda_radvol",
    "NNODES_MODEL": 4,
    "NNODES_ENDA": 8
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "radarvol": True, "membrange": "0-40",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers, EndaAnalysis],
                    "preproc_wt":"00:20:00", "model_wt": "01:00:00"}).getconf()
enda_radvol = ModelSuite("cosmo_2I_enda_radvol")
basicenv.add_to(enda_radvol.suite)
day = enda_radvol.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[0])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 1):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

enda_radvol.check()
enda_radvol.write(interactive=interactive)
enda_radvol.replace(interactive=interactive)


# Suite fcruc
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/fcruc",
    "NNODES_MODEL": 16,
    "NNODES_ENDA": 4
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": False, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "runlist": [GetObs, EpsMembers],
                    "preproc_wt":"00:30:00", "model_wt": "01:00:00"}).getconf()
fcruc = ModelSuite("cosmo_2I_fcruc")
basicenv.add_to(fcruc.suite)
day = fcruc.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[1])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 3):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

fcruc.check()
fcruc.write(interactive=interactive)
fcruc.replace(interactive=interactive)

# Suite fcens
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/fcens",
    "NNODES_MODEL": 7,
    "NNODES_ENDA": 4,
    "ECF_TIMEOUT": "14400"
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": False, "lhn": True, "membrange": "1-20",
                    "postprocrange": "1-20",
                    "runlist": [GetObs, EpsMembers, EpsPostproc]}).getconf()
fcens = ModelSuite("cosmo_2I_fcens")
basicenv.add_to(fcens.suite)
day = fcens.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[2])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(21, 24, 3): # h=21
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

fcens.check()
fcens.write(interactive=interactive)
fcens.replace(interactive=interactive)

# repeat fcens for generating a recover suite
conf.update({"startmethod": "manual"})
fcens = ModelSuite("cosmo_2I_fcens_recover")
basicenv.add_to(fcens.suite)
day = fcens.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=365)).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(21, 24, 3): # h=21
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

fcens.check()
fcens.write(interactive=interactive)
fcens.replace(interactive=interactive)


# Suite enda_dia
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/cosmo_2I/enda_dia",
})
basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"runlist": [EndaDiagnostics], "startmethod": "starttime_cron",
                    "starttime": "16:26"}).getconf()
enda_dia = ModelSuite("cosmo_2I_enda_dia")
basicenv.add_to(enda_dia.suite)
WaitAndRun(dep=None, conf=conf).add_to(enda_dia.suite)

enda_dia.check()
enda_dia.write(interactive=interactive)
enda_dia.replace(interactive=interactive)

