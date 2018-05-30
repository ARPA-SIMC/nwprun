import ecflow
import os

# from https://www.google.com/url?q=https://rosettacode.org/wiki/Range_expansion%23Python&sa=D&ust=1517307153586000&usg=AFQjCNHTzzU1Kk2XfryWWIoWwKTWNWItiw
def rangeexpand(txt):
    lst = []
    for r in txt.split(','):
        if '-' in r[1:]:
            r0, r1 = r[1:].split('-', 1)
            lst += range(int(r[0] + r0), int(r1) + 1)
        else:
            lst.append(int(r))
    return lst


def expr_or(expr, orexpr):
    if expr == "":
        return orexpr
    elif orexpr == "":
        return expr
    else:
        return expr+" || "+orexpr


def ask_confirm(msg=""):
    ans = raw_input(msg+" (y/n)? ")
    return ans.startswith("y")


def daily_cron(step):
    if step > 60 or step < 0:
        return None
    time_series = ecflow.TimeSeries(ecflow.TimeSlot(0, 0),
                                    ecflow.TimeSlot(23, 60-step),
                                    ecflow.TimeSlot(0,step), False)
    cron = ecflow.Cron()
    cron.set_time_series(time_series)
    return cron

class SchedEnv:
    def __init__(self, sched="sh"):
        self.sched = sched
    
    def add_to(self, node):
        if self.sched is not None:
            if self.sched == "pbs":
                node.add_variable("ECF_JOB_CMD", "qsub %EXTRA_SCHED% %ECF_JOB%")
                node.add_variable("ECF_KILL_CMD", "qdel %ECF_RID%")
                node.add_variable("ECF_STATUS_CMD", "qstat %ECF_RID%")
            elif self.sched == "slurm":
                node.add_variable("ECF_JOB_CMD", "(date -u '+%%Y%%m%%d%%H%%M'; sbatch %EXTRA_SCHED% %ECF_JOB%) >>%ECF_HOME%/sbatch.log 2>&1")
                node.add_variable("ECF_KILL_CMD", "scancel %ECF_RID%")
                node.add_variable("ECF_STATUS_CMD", "squeue -j %ECF_RID%")
            elif self.sched == "sh":
                node.add_variable("ECF_JOB_CMD", "%ECF_JOB% 1> %ECF_JOBOUT% 2>&1")
                node.add_variable("ECF_KILL_CMD", "kill -15 %ECF_RID%")
                node.add_variable("ECF_STATUS_CMD", "ps --sid %ECF_RID% -f")

class GetObs:
    def __init__(self, gts=True, lhn=True):
        self.gts = gts
        self.lhn = lhn

    def add_to(self, node):
        fam = node.add_family("get_obs") # experimental is it complete if empty?
        if self.gts or self.lhn:
#            fam = node.add_family("get_obs")
            if self.gts: fam.add_task("get_gts")
            if self.lhn: fam.add_task("get_radarlhn")

class Preproc:
    def __init__(self, modelname="cosmo"):
        if modelname == "cosmo":
            self.modelname = "int2lm"
        else:
            self.modelname = "pre" + modelname

    def add_to(self, node):
        fam = node.add_family("preproc")
        fam.add_trigger("./check_memb:required == 2")
        task = fam.add_task("get_parent")
        SchedEnv("sh").add_to(task) # interactive because net access required for galileo
        task = fam.add_task(self.modelname).add_trigger("./get_parent == complete")
        task.add_variable("WALL_TIME", "01:00:00")
        fam.add_task("merge_analysis").add_trigger("./"+self.modelname+" == complete")

class Model:
    def __init__(self, postproc=True, postproctype="async", modelname="cosmo", wait_obs=True):
        self.postproc = postproc
        self.modelname = modelname
        self.wait_obs = wait_obs
        self.postproctype = postproctype

    def add_to(self, node):
        fam = node.add_family("model")
        trig = "./preproc == complete"
        if self.wait_obs: trig+= " && ../../get_obs == complete"
        fam.add_trigger(trig)
        fam.add_variable("WALL_TIME", "03:00:00")
        fam.add_task(self.modelname).add_event("started")
        if self.postproc:
            if self.postproctype == "async":
                fam.add_task("postproc").add_trigger("./"+self.modelname+":started == set")
            else:
                fam.add_task("postproc").add_trigger("./"+self.modelname+" == complete")

class EpsMembers:
    def __init__(self, membrange="0", nofail=False, modelname="cosmo", postprocrange=None, postproctype="async", wait_obs=True, timer=None):
        self.membrange = rangeexpand(membrange)
        self.nofail = nofail
        self.modelname = modelname
        if postprocrange is None: self.postprocrange = self.membrange
        else: self.postprocrange = rangeexpand(postprocrange)
        self.postproctype = postproctype
        self.wait_obs = wait_obs
        self.timer = timer

    def add_to(self, node):
        ensfam = node.add_family("eps_members")
        for eps_memb in self.membrange:
            if eps_memb == 0:
                fname = "deterministic" # deterministic
            else:
                fname = "eps_member_"+str(eps_memb)
            fam = ensfam.add_family(fname)
            fam.add_complete(fname+"/check_memb:required == 1")
            fam.add_variable("ECF_ENS_MEMB", str(eps_memb))
            if self.nofail:
                fam.add_variable("NO_FAIL", "TRUE") # do not fail in case of error
            task = fam.add_task("check_memb").add_meter("required", 0, 2)
            SchedEnv(sched="sh").add_to(task)
            Preproc(modelname=self.modelname).add_to(fam)
            Model(postproc=(eps_memb in self.postprocrange),
                  postproctype=self.postproctype,
                  modelname=self.modelname, wait_obs=self.wait_obs).add_to(fam)

            wipe = fam.add_family("wipe")
            SchedEnv(sched="sh").add_to(wipe)
            timerdep = ""
            if self.timer is not None:
                task = wipe.add_task("wipe_timer")
                task.add_complete("./wipe_member == complete")
                task.add_time(self.timer)
                task.add_variable("ECF_DUMMY_TASK","Y")
                timerdep = " || ./wipe_timer == complete"

            task = wipe.add_task("wipe_member")
            task.add_complete("../model == complete")
            task.add_trigger("../model == aborted || ../preproc == aborted || ../check_memb == aborted"+timerdep)

class EndaAnalysis:
    def __init__(self):
        pass

    def add_to(self, node):
        fam = node.add_family("enda_analysis")
        fam.add_trigger("./eps_members == complete")
        fam.add_task("prepare_kenda")
        task = fam.add_task("kenda").add_trigger("./prepare_kenda == complete")
        task.add_variable("WALL_TIME", "00:20:00")
        fam.add_task("archive_kenda").add_trigger("./kenda == complete")

class ContinuousAnalysis:
    def __init__(self):
        pass

    def add_to(self, node):
        fam = node.add_family("continuous_analysis")
        fam.add_trigger("./eps_members == complete")
        fam.add_task("archive_analysis")

class EpsPostproc:
    def __init__(self):
        pass

    def add_to(self, node):
        fam = node.add_family("eps_postproc")
        fam.add_trigger("./eps_members == complete")
        fam.add_task("compute_prob")

# add a wipe family containing a single task wipe; wipe should set run
# family to complete, possibly with more fine-grain control on tasks,
# and exiting thus resubmitting the suite for next run
class WipeRun:
    def __init__(self, runlist=[], timer=None):
        self.runlist = runlist
        self.timer = timer

    def add_to(self, node):
        timerdep = ""
        if self.timer is not None:
            task = wipe.add_task("wipe_timer")
            task.add_complete("./wipe_run == complete")
            task.add_time(self.timer)
            task.add_variable("ECF_DUMMY_TASK","Y")
            timerdep = "./wipe_timer == complete"

        trig = ""
        for fam in self.runlist:
            if isinstance(fam, GetObs):
                trig = expr_or(trig, "../run/get_obs == aborted")
            elif isinstance(fam, EndaAnalysis):
                trig = expr_or(trig, "../run/enda_analysis == aborted")
            elif isinstance(fam, ContinuousAnalysis):
                trig = expr_or(trig, "../run/continuous_analysis == aborted")
            elif isinstance(fam, EpsPostproc):
                trig = expr_or(trig, "../run/eps_postproc == aborted")

        fulldep = expr_or(trig, timerdep)
        if fulldep != "":
            wipe = node.add_family("wipe")
            SchedEnv(sched="sh").add_to(wipe)
            task = wipe.add_task("wipe_run")
            task.add_complete("../run == complete")
            task.add_trigger(fulldep) # || ../check_run == aborted")

class BasicEnv():
    def __init__(self, srctree=None, worktree=None, sched=None, client_wrap="", ntries=1, extra_env=None):
        self.srctree = srctree
        self.worktree = worktree
        self.sched = sched
        if client_wrap == "": self.ecflow_client = "ecflow_client"
        else: self.ecflow_client = client_wrap+" ecflow_client"
        self.ntries = ntries
        self.extra_env = extra_env

    def add_to(self, node):
        if self.sched is not None:
            sched_suff = "_"+self.sched
        else:
            sched_suff = ""
        node.add_variable("ECF_INCLUDE", os.path.join(self.srctree,"ecflow","include"+sched_suff))
        node.add_variable("ECF_FILES", os.path.join(self.srctree,"ecflow","jobs"))
        node.add_variable("BASEDIR", self.srctree)
        node.add_variable("ECF_HOME", self.worktree)
        node.add_variable("ecflow_client", self.ecflow_client)
        node.add_variable("ECF_TRIES", str(self.ntries))
        SchedEnv(sched=self.sched).add_to(node)
        if self.extra_env is not None:
            for var in self.extra_env:
                node.add_variable(var, self.extra_env[var])

class WaitAndRun:
    def __init__(self, dep=None, time=None, runlist=None, timer=None, cronfreq=10):
        self.dep = dep
        self.time = time
        self.runlist = runlist
        self.timer = timer
        self.cronfreq = cronfreq

    def add_to(self, node):
        if self.time is None:
            fam = node.add_family("check_run")
            SchedEnv("sh").add_to(fam)

            fam.add_complete("check_run/can_run == complete")
            # first task
            task = fam.add_task("can_run")
            task.add_trigger("check_run:checked")
            task.add_event("ready")
            # second task
            task = fam.add_task("check_run")
            task.add_complete("can_run:ready")
            task.add_cron(daily_cron(self.cronfreq))
            if self.dep is not None:
                task.add_trigger("../../"+self.dep+" == complete")
            task.add_event("checked")

        fam = node.add_family("run")
        if self.dep is not None and self.time is not None:
            fam.add_trigger("../"+self.dep+" == complete")
        if self.time is not None:
            fam.add_time(self.time) # replace with today, to test
        else:
            fam.add_trigger("check_run == complete")

        if self.runlist is not None:
            for run in self.runlist:
                run.add_to(fam)
        WipeRun(runlist=self.runlist, timer=self.timer).add_to(node)

class ModelSuite():
    def __init__(self, name):
        self.name = name
        self.defs = ecflow.Defs()
        self.suite = self.defs.add_suite(name)
        self.checked = False


    def check(self):
        # check syntax
        result = self.defs.check()
        if result != "":
            print("Error in "+self.name+" suite definition:")
            print(result)
        else:
            # check job tree
            result = self.defs.check_job_creation()
            if result != "":
                print("Error in "+self.name+" suite job creation:")
                print(result)
            else: 
                self.checked = True


    def write(self, interactive=True):
        if not self.checked:
            print("suite "+self.name+" has not been checked, refusing to write")
            return
        name = self.name+".def"
        if interactive:
            if os.path.exists(name):
                if not ask_confirm("Definition file "+name+" exists, replace"):
                    return
        self.defs.save_as_defs(name)
        print("Suite saved in "+name)
        

    def replace(self, interactive=True):
        if not self.checked:
            print("suite "+self.name+" has not been checked, refusing to replace")
            return
        if interactive:
            if not ask_confirm("Replace suite "+self.name+" on server"):
                return
        client = ecflow.Client() # connect using environment
        client.replace("/"+self.name, self.defs)
        print("Suite "+self.name+" replaced on server")
