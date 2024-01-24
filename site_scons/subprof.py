#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" Gather cpu, memory, etc usage (profiling data) for a given
   (sub)process.

"""

import collections
import functools
import os
import pathlib
import platform
import signal
import subprocess
import sys
import time
import unittest
import warnings

FIELDS = [
    # order here is based on how I want the columns to show up in the output.
    'git_id',     # git commit ID so you can find relevsant changes
    'cpu_total',  # sys + user cpu seconds
    'cpu_user',   # user space cpu seconds
    'cpu_system', # system cpu seconds
    'max_rss',    # max resident size (rss) in memory
    'timestamp',  # unix epoch time at which this was created
                  #   (typically at the end of the process)
    'hostname',   # uname perversely calls this "node"
    'arch',       # eg "x86_64" (uname calls this "machine")
    'os',         # eg "Darwin 20.4.0",
    'exit_code',  # whatever was passed to exit()
    'signal',     # name of signal which killed this (or blank)
]

def signal_str(waitstatus):
    if not os.WIFSIGNALED(waitstatus):
        return "-"

    signum = os.WTERMSIG(waitstatus),
    str = "?"
    try:
        str = signal.strsignal(signum),
    finally:
        return str

def exit_code_str(waitstatus):
    str = "?"
    try:
        str = os.waitstatus_to_exitcode(waitstatus),
    finally:
        return str[0]

def fmt_secs(secs):
    #return round(secs, 6)
    #return format(secs, '0>.6')
    return format(secs, 'f')

# using named tuple here because I'm imagining dumping this
# as csv/tsv or slapping it in a column database or something.
class Profile(collections.namedtuple("Subprof", FIELDS)):
    def __new__(cls, pid, waitstatus, rusage):
        uname = platform.uname()
        return super().__new__(
            _cls       = cls,
            timestamp  = int(time.time()),
            cpu_total  = fmt_secs(rusage.ru_utime + rusage.ru_stime),
            cpu_user   = fmt_secs(rusage.ru_utime),
            cpu_system = fmt_secs(rusage.ru_stime),
            max_rss    = rusage.ru_maxrss,
            exit_code  = exit_code_str(waitstatus),
            signal     = signal_str(waitstatus),
            git_id     = _git_commit_id(),
            hostname   = uname.node,
            arch       = uname.machine,
            os         = f"{uname.system} {uname.release}",
        )

    """ write_tsv(filename) - append profiling data to a file
        as excel-style tab separated values, creating that file
        (including the full directory path) if necessary.
    """
    def write_tsv(self, filename):
        full_name = pathlib.Path(filename).resolve()
        pathlib.Path.mkdir(full_name.parent, parents=True, exist_ok=True)
        with open(full_name, mode='a') as outf:
            out = csv.writer(outf, dialect="excel-tab")

            if outf.tell() == 0:
                # "appending" at the start of file, so
                # it wants a header:
                out.writerow(FIELDS)

            out.writerow(self)

# example rusage from wait4():
#  resource.struct_rusage(
#    ru_utime=0.013462,
#    ru_stime=0.015332,
#    ru_maxrss=9834496,
#    ru_ixrss=0,
#    ru_idrss=0,
#    ru_isrss=0,
#    ru_minflt=4457,
#    ru_majflt=0,
#    ru_nswap=0,
#    ru_inblock=0,
#    ru_oublock=0,
#    ru_msgsnd=0,
#    ru_msgrcv=0,
#    ru_nsignals=0,
#    ru_nvcsw=20,
#    ru_nivcsw=46
#  )

@functools.cache
def _git_commit_id():

    git_id = "[unavailable]"
    try:
        # this is a good-enough stab at getting the git commit ID,
        # and doesn't require anyone to install any special modules.
        git_result = subprocess.run(
            ['git', 'log', '-1', '--pretty=format:%h'],
            stdout=subprocess.PIPE, check=True
        )
        git_id = git_result.stdout.decode('ascii')
    except Exception as ex:
        warnings.warn(f"can't get git commit id: {ex}")

    return git_id

#######  testing #######
import csv
import resource
class Testsubprof(unittest.TestCase):
    def test_all(self):
        csvo = csv.writer(sys.stdout, dialect="excel-tab");
        data = Profile(os.getpid(), 0x10f, resource.getrusage(resource.RUSAGE_SELF))
        csvo.writerow(data._fields)
        csvo.writerow(data)
        # ... uhh let's say if it didn't crash, it worked.

if __name__ == '__main__':
    unittest.main()

