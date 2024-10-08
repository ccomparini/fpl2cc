#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

Gather cpu, memory, etc usage (profiling data) for a given
(sub)process.

This is used to generate the files we dump in the yprof directory.

"""

import collections
import functools
import inspect
import os
import pathlib
import platform
import signal
import subprocess
import sys
import time
import unittest

FIELDS = [
    # order here is based on how I want the columns to show up in the output.
    'git_id',     # git commit ID so you can find relevant changes
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

def warn_here(msg):
    whence = inspect.stack()[1]
    print(f"Warning ({whence.filename}:{whence.lineno}) {msg}", file=sys.stderr)

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

def fmt_commit_id(cid):
    if cid is None:
        warn_here(f"no commit ID available - using 0000000")
        return '0000000'
    return cid

def fmt_secs(secs):
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
            git_id     = fmt_commit_id(_git_commit_id()),
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

# returns true if git says the working tree matches the HEAD;
# false otherwise (including cases where we're not even in
# a git tree)
def _git_is_at_head(*paths):
    try:
        git_result = subprocess.run(
            ['git', 'diff-index', 'HEAD', '--quiet', *paths]
        )
    except Exception as ex:
        warn_here(f"\nerror on git diff-index: {ex}")

    return git_result.returncode == 0


@functools.cache
def _git_commit_id():

    git_id = None
    try:
        # this is a good-enough stab at getting the git commit ID,
        # and doesn't require anyone to install any special modules.
        git_result = subprocess.run(
            ['git', 'log', '-1', '--pretty=format:%h'],
            stdout=subprocess.PIPE, check=True
        )
        git_id = git_result.stdout.decode('ascii')
        if not _git_is_at_head(':!yprof'):
            # the working tree (except the yprof dir) doesn't match
            # the HEAD, but we're profiling code on the working tree
            # (i.e. on disk), so we want to be clear that the git ID
            # doesn't really match what we ran against.  So, warn,
            # and add an asterisk to the end:
            warn_here(f"git working tree does not match HEAD")
            git_id += '*'
    except Exception as ex:
        warn_here(f"\ncan't get git commit id: {ex}")

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

