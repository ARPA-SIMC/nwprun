#!/usr/bin/python3

import os,sys
import datetime
import optparse
import ecflow
from nwprun import *

parser = optparse.OptionParser(usage="%prog [OPTIONS]")
parser.add_option("--yes", help="work in non-interactive mode and answer yes to all questions (it will overwrite files and replace scripts on server)",
                  action="store_true")
parser.add_option("--delta", help="delta time in days to go back", default=0)

opts, args = parser.parse_args()
interactive = not opts.yes
delta = opts.delta

common_extra_env = {
    "NO_FAIL": "FALSE",
    "TASK_PER_CORE": "1",
    "HPCENV": os.environ["HPC_SYSTEM"],
    "ECF_TIMEOUT": "7200",
    "ECF_DENIED": "",
    "WALL_TIME_WAIT": "04:10:00",
    "WALL_TIME_ARCHIVE": "04:10:00"
}

# Suite icon fci2i
extra_env = common_extra_env.copy()
extra_env.update({
    "NWPCONF": "prod/icon_2I/fci2i",
    "NNODES_PREMODEL": 2,
    "NNODES_MODEL": 8,
    "NTASKS_PREMODEL": 64,
    "NTASKS_MODEL": 256,
    "NTASKS_POSTPROC": 1,
    "WALL_TIME_PREMODEL": "00:20:00",
    "WALL_TIME_MODEL": "03:00:00"
})
basicenv = BasicEnv(srctree=os.path.join(os.environ["WORKDIR_BASE"], "nwprun"),
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="slurm",
                    client_wrap="",
                    ntries=2,
                    extra_env=extra_env)

conf = ModelConfig({"gts": False, "lhn": True, "membrange": "0",
                    "postprocrange": "0",
                    "modelname": "icon", 
                    "runlist": [GetObs, EpsMembers]}).getconf()
icon = ModelSuite("icon_2I_fci2i")
basicenv.add_to(icon.suite)
day = icon.suite.add_family("day").add_repeat(
    ecflow.RepeatDate("YMD", 
                      int((datetime.datetime.now()-datetime.timedelta(days=delta)).strftime("%Y%m%d")),
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


