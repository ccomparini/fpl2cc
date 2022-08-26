import sysconfig
import os
import subprocess
import sys

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '

fpl_include_dirs = [ 'src/grammarlib' ]

# figure out some sensible way to set this.
# 10 is great on my ~2015 macbook pro, but utterly destroys my
# dinky amazon lightsail instance.
#SetOption('num_jobs', 4)
SetOption('num_jobs', 10)

ccflags = "-std=c++20 -Wno-parentheses"
if debugger : ccflags += " -g"
else        : ccflags += " -O2"

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
    FPLPATH = fpl_include_dirs,
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
    for fn in target:
        with open(fn.abspath, 'a'):
            os.utime(fn.abspath, None)
    return 0

# CompareOut is a "builder" which compares the source files (actually
# 2 test output files - one with whatever was generated and one with
# what we expected it to generate) and creates the target file (.success)
# and returns success if they match, or reports failure otherwise.
env.Append(BUILDERS =
    { 'CompareOut' : Builder(
        action=sources_are_same,
        suffix = '.success',
        src_suffix = '.out') } )

def depend_on_fpl2cc(target, source, env) :
    # make fpl2cc a dependency of each source (.fpl) file passed
    # (not a source dependency!).  This is because fpl2cc must be
    # built -before- the source (.fpl) file is processed.  If we
    # simply add fpl2cc to the source dependency list, scons thinks
    # it can process the fpl in parallel with building fpl2cc,
    # which leads to races and (usually) the fpl file being
    # processed using the prior version of fpl2cc.
    for src in source :
        Depends(src, '#bin/fpl2cc') # deliberate: .fpl file depends on fpl2cc
    return target, source



# fpl -> cc builder:
fpl_args = '--src-path=' + ':'.join(fpl_include_dirs) + ' $FPLOPTS $SOURCES --out $TARGET --depfile .deps --statedump .states'
env.Append(BUILDERS =
    { 'Fpl2cc' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
                 emitter = depend_on_fpl2cc,
	         suffix = '.cc',
	         src_suffix = '.fpl') } )

# fpl -> h builder:
env.Append(BUILDERS =
    { 'Fpl2h' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
                 emitter = depend_on_fpl2cc,
	         suffix = '_parser.h',
	         src_suffix = '.fpl') } )

# fpl -> jest builder:
env.Append(BUILDERS =
    { 'Fpl2jest' : Builder(action = debugger + 'bin/fpl ' + fpl_args,
	         suffix = '.jest',
	         src_suffix = '.fpl') } )

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
env.Append(BUILDERS =
    { 'Jemp2h' : Builder(action = 'bin/jemplpl $SOURCE > $TARGET',
	         suffix = '.h',
	         src_suffix = '.h.jemp') } )

src_subdirs = [
    # these are specified roughly in compile/dependency order:
    'util/',
    'util/test/',
    'fpl2cc/',
    'fpl2cc/test/',
    'compiler/',
]
for sdir in src_subdirs: 
    SConscript('src/' + sdir + 'SConstruct', exports='env', variant_dir='build/'+sdir, duplicate=False)

