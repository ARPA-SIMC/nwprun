#!/usr/bin/python3

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

hpcenv = os.environ["HPC_SYSTEM"]
common_extra_env = {
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": os.environ["HPC_SYSTEM"],
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": "",
    "WALL_TIME_WAIT": "04:10:00",
    "WALL_TIME_ARCHIVE": "04:10:00"
}


# Suite icon verif_mod
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/icon_2I/verif_mod",
    "NNODES_MEC": 6,
    "WALL_TIME_MEC": "00:30:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["WORKDIR_BASE"], "nwprun","ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": False, "membrange": "0", "modelname": "icon",
                    "runlist": [GetObs, GetModel, Verification]}).getconf()
icon = ModelSuite("icon_2I_verif_mod")
basicenv.add_to(icon.suite)
day = icon.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[0])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

icon.check()
icon.write(interactive=interactive)
icon.replace(interactive=interactive)


# Suite icon verif_obs
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/icon_2I/verif_obs",
    "NNODES_MEC": 6,
    "WALL_TIME_MEC": "00:30:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["WORKDIR_BASE"], "nwprun","ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": False, "membrange": "0", "modelname": "icon",
                    "runlist": [GetObs, GetModel, Verification]}).getconf()
icon = ModelSuite("icon_2I_verif_obs")
basicenv.add_to(icon.suite)
day = icon.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[1])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 1):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

icon.check()
icon.write(interactive=interactive)
icon.replace(interactive=interactive)


# Suite ifs verif_obs
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/ifs/verif_obs",
    "NNODES_MEC": 6,
    "WALL_TIME_MEC": "00:30:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["WORKDIR_BASE"], "nwprun","ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": False, "membrange": "0", "modelname": "ifs",
                    "runlist": [GetObs, GetModel, Verification]}).getconf()
ifs = ModelSuite("ifs_verif_obs")
basicenv.add_to(ifs.suite)
day = ifs.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD",
                      int((datetime.datetime.now()-datetime.timedelta(days=delta[2])).strftime("%Y%m%d")),
                      20301228))

hdep = None # first repetition has no dependency
for h in range(0, 24, 1):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

ifs.check()
ifs.write(interactive=interactive)
ifs.replace(interactive=interactive)

