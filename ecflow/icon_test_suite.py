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

common_extra_env = {
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": os.environ["HPC_SYSTEM"],
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": ""
}

# Suite icon test
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "test/icon_2I/fcast",
    "NNODES_PREMODEL": 3,
    "NNODES_MODEL": 8,
    "NNODES_ENDA": 6
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["WORKDIR_BASE"], "nwprun","ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": False, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "modelname": "icon", 
                    "runlist": [GetObs, EpsMembers],
                    "preproc_wt":"00:20:00", "model_wt": "03:00:00"}).getconf()
icont = ModelSuite("icon_2I_test")
basicenv.add_to(icont.suite)
icont.suite.add_variable("ECF_FILES", os.path.join(os.environ["WORKDIR_BASE"],"nwprun","ecflow","jobs_test"))

day = icont.suite.add_family("day")
icont.suite.add_variable("YMD",20240101)

hdep = None # first repetition has no dependency
for h in range(0, 24, 12):
    famname = "hour_" + ("%02d" % h)
    hour = day.add_family(famname).add_variable("TIME", "%02d" % h)
    #    hrun = "%02d:00" % (h+1 % 24) # start 1h after nominal time
    WaitAndRun(dep=hdep, conf=conf).add_to(hour)
    hdep = famname # dependency for next repetition

icont.check()
icont.write(interactive=interactive)
icont.replace(interactive=interactive)


# Suite enda test
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "test/icon_2I/enda",
    "NNODES_PREMODEL": 1,
    "NNODES_MODEL": 3,
    "NNODES_ENDA": 6
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap=os.path.join(os.environ["WORKDIR_BASE"], "nwprun","ecflow","ec_wrap"),
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": True, "lhn": True, "radarvol": True, "membrange": "0-40",
                    "postprocrange": "-1",
                    "modelname": "icon",
                    "runlist": [GetObs, EpsMembers, EndaAnalysis],
                    "preproc_wt":"00:20:00", "model_wt": "01:00:00"}).getconf()
enda = ModelSuite("icon_2I_enda_test")
basicenv.add_to(enda.suite)
day = enda.suite.add_family("day").add_repeat(
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

enda.check()
enda.write(interactive=interactive)
enda.replace(interactive=interactive)

