#!/usr/bin/python3

import os,sys
#import datetime
import optparse
import ecflow
from nwprun import *

parser = optparse.OptionParser(usage="%prog [OPTIONS]")
parser.add_option("--yes", help="work in non-interactive mode and answer yes to all questions (it will overwrite files and replace scripts on server)",
                  action="store_true")

opts, args = parser.parse_args()
interactive = not opts.yes

suitename = "cron_get"
famlist = ["cosmo_5M_assimm18_cineca", "cosmo_5M_assimm12_cineca",
           "cosmo_5M_fcast_cineca", "cosmo_2I_fcast_cineca", "cosmo_am_enda",
           "cosmo_am_eps", "hres_am_foricon", "ifsens_am_enda",
           "ifsens_am_endabak", "gts_bufr", "radar_lhn", "radar_sri",
           "radar_vol"]


basicenv = BasicEnv(srctree=os.environ["OPE"],
                    worktree=os.path.join(os.environ["WORKDIR_BASE"], "ecflow"),
                    sched="sh",
                    client_wrap=os.path.join(os.environ["OPE"],"ecflow","ec_wrap"),
                    ntries=2,
                    extra_env={"ECF_PASS": "FREE", "ECF_DUMMY_TASK": "", "ECF_TIMEOUT": "7200", "ECF_DENIED": ""})


defs = ecflow.Defs()
suite = defs.add_suite(suitename)
basicenv.add_to(suite)

for fam in famlist:
    suite.add_task(fam).add_label("currdate", "no data").add_label("lastdate", "no data").add_variable("ECF_PASS", "FREE")

name = suitename+".def"

if interactive:
    if os.path.exists(name):
        if not ask_confirm("Definition file "+name+" exists, replace"):
            sys.exit(0)
defs.save_as_defs(name)
print("Suite saved in "+name)

if interactive:
    if not ask_confirm("Replace suite "+suitename+" on server"):
        sys.exit(0)
client = ecflow.Client() # connect using environment
client.replace("/"+suitename, defs)
print("Suite "+suitename+" replaced on server")
