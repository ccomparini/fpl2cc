import atexit
import os
import sys
import sysconfig

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '


cpu_count = os.cpu_count();
if(cpu_count is None):
    cpu_count = 1  # we are currently running on something, after all..
SetOption('num_jobs', cpu_count + 1);


# on ubuntu, I had to:
#   sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/gcc-10 100
# to get a working c++20 compiler
ccflags = "-std=c++20 -Wno-parentheses"
if debugger : ccflags += " -g"
# commenting out optimizations for now because fpl-generated test
# code takes forever to compile:
#else        : ccflags += " -O2"

# if you tell scons:
#    tools = [ 'default', 'clangxx', ],
# .. it seems to  always try to use clangxx.  not sure why.
# what we want is to use clang if it's available, but otherwise
# use default (i.e. probably gnu).
def toolset():
    # let's try asking python what it was compiled with,
    # and seeing if it looks like clang.  this should be
    # faster than searching the PATH or whatever for a compiler
    # and then running the compiler to see what it is...
    cxx = sysconfig.get_config_var('CXX')
    if 'clang' in cxx:
        # .. you still need 'default' here or scons gets twisted
        return [ 'default', 'clangxx' ]

    # ok, probably no clang.  use default.
    return [ 'default' ]


#    PLATFORM=platform,
#    BINDIR="#export/foo/bin",
#    INCDIR=include,
#    LIBDIR=lib,
#    CPPPATH=[include],
#    LIBPATH=[lib],
#    LIBS='..whatever',
env = Environment(
    CCFLAGS = ccflags,
    CPPPATH = [
        '.',
        '#src',
        '#src/util',
    ],
    LIBPATH=[ '#lib' ],
    LIBS='jest_util',
    tools = toolset(),
)

def read_dependencies(senv):
    for dirpath, dirnames, filenames in os.walk('.'):
        for filename in filenames:
            if filename.endswith('.deps'):
                senv.ParseDepends(os.path.join(dirpath, filename))

read_dependencies(env)

# action-compatible comparison function to check if all source
# files have the same content.  used for comparing output in tests.
# prints an error message and returns 1 (= fail) if there's
# a mismatch.
# Otherwise, creates/touches the target file(s) and returns 0
# (= success)
def sources_are_same(target, source, env):
    last_contents = None
    for fn in source:
        with open(fn.abspath, mode='rb') as f: contents = f.read()
        if last_contents is not None:
            if contents != last_contents:
                sys.stderr.write("MISMATCH: {a} {b}\n".format(a=last_fn.path, b=fn.path))
                return 1
        last_contents = contents
        last_fn = fn

    # success, so "touch" all targets (i.e. make them exist
    # and  set the utime to the current time).
    # (typically there's only one target file, called xxx.success...)
    for fn in target :
        with open(fn.abspath, 'a'):
            os.utime(fn.abspath, None)
    return 0

# CompareOut is a "builder" which compares the source files (actually
# 2 test output files - one with whatever was generated and one with
# what we expected it to generate) and creates the target file (.success)
# and returns success if they match, or reports failure otherwise.
env.Append(BUILDERS = {
    'CompareOut' : Builder(
        action=sources_are_same,
        suffix = '.success',
        src_suffix = '.out'
    )
})

env.Append(BUILDERS = {
    'Fpl2cc' : Builder(
        action = fpl_compile_command(),
        emitter = depend_on_fpl2cc(),
	suffix = '.cc',
	src_suffix = '.fpl'
    )
})

# fpl -> h builder:
env.Append(BUILDERS = {
    'Fpl2h' : Builder(
        action = fpl_compile_command(),
        emitter = depend_on_fpl2cc(),
	suffix = '_parser.h',
	src_suffix = '.fpl'
    )
})


# another fake "Scanner" to make it so that headers generated
# from .jemp sources depend on jemplpl.
# there must be a simpler way to do this...
def depend_on_jemplpl(node, env, path) :
    return [ '#bin/jemplpl' ]

env.Append(
    SCANNERS = Scanner(
        function = depend_on_jemplpl,
        skeys = ['.jemp']
    )
)

# jemp -> h builder
env.Append(BUILDERS = {
    'Jemp2h' : Builder(
        action = 'bin/jemplpl $SOURCE > $TARGET',
	suffix = '.h',
	src_suffix = '.h.jemp'
    )
})

src_subdirs = [
    # these are specified roughly in compile/dependency order:
    'util/',
    'util/test/',
    'fpl2cc/',
    'fpl2cc/test/',
    'grammarlib/test/',
    'compiler/',
]

for sdir in src_subdirs: 
    SConscript('src/' + sdir + 'SConstruct', exports='env', variant_dir='build/'+sdir, duplicate=False)

# Stolen from:
#  https://scons.org/doc/production/HTML/scons-user.html#idp105548894836432
# .. uhh and then I did a source dive to figure out how to make
# it print something useful...
def print_build_failures():
    from SCons.Script import GetBuildFailures
    fails = GetBuildFailures()
    if len(fails):
        print(f"\n\n{len(fails)} FAILURES:");
        for bf in fails:
            print(f"    {bf.node}\t{env.subst(bf.action, executor=bf.executor)}")

atexit.register(print_build_failures)

