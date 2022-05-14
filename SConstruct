from pprint import pprint
from pathlib import Path
import sys

# TODO you _can_ get the target files to not be slapped into
# the source tree!  Maybe?
# https://scons.org/doc/4.3.0/HTML/scons-user.html#chap-separate

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '

fpl_include = ' --src-path=src/fpl2cc/grammarlib '

SetOption('num_jobs', 10)

ccflags = ''
if debugger : ccflags += " -g"
else        : ccflags += " -O2"
ccflags += " -std=c++20 -Wno-parentheses"

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
    tools = [ 'default', 'clangxx', ],
)


# fake "Scanner" to make it so that cc files generated
# by fpl files implicitly depend on fpl2cc:
def depend_on_fpl2cc(node, env, path) :
    return [ '#bin/fpl2cc' ]

env.Append(
    SCANNERS = Scanner(
        # this is wrong for Fpl2jest.  sigh.
        # can't I force the dependency in the builder?
        function = depend_on_fpl2cc,
        skeys = ['.fpl']
    )
)

# action-compatible comparison function to check if all source
# files have the same content.  used for comparing output in tests.
# prints an error message and returns 1 (= fail) if there's
# a mismatch.
# Otherwise, creates the target file(s) and returns 0 (= success)
def sources_are_same(target, source, env):
    last_contents = None
    for fn in source:
        with open(fn.abspath) as f: contents = f.readlines()
        if last_contents is not None:
            if contents != last_contents:
                sys.stderr.write("MISMATCH: {a} {b}\n".format(a=fn.path, b=last_fn.path))
                return 1
        last_contents = contents
        last_fn = fn

    for fn in target:
        Path(fn.abspath).touch()
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

# fpl -> cc builder:
# at this point this is for testing fpl
env.Append(BUILDERS =
    { 'Fpl2cc' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_include + ' $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.cc',
	         src_suffix = '.fpl') } )

# fpl -> h builder:
env.Append(BUILDERS =
    { 'Fpl2h' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_include + ' $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.h',
	         src_suffix = '.fpl') } )

# fpl -> jest builder:
env.Append(BUILDERS =
    { 'Fpl2jest' : Builder(action = debugger + 'bin/fpl ' + fpl_include + ' $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.jest',
	         src_suffix = '.fpl') } )

# another fake "Scanner" to make it so that headers generated
# from .jemp sources depend on jemplate.
# there must be a simpler way to do this...
def depend_on_jemplate(node, env, path) :
    return [ '#bin/jemplate' ]
env.Append(
    SCANNERS = Scanner(
        function = depend_on_jemplate,
        skeys = ['.jemp']
    )
)


# jemp -> h builder
env.Append(BUILDERS =
    { 'Jemp2h' : Builder(action = 'bin/jemplate $SOURCE > $TARGET',
	         suffix = '.h',
	         src_suffix = '.jemp') } )

src_subdirs = [
    # these are specified roughly in compile/dependency order:
    'util/',
    'util/test/',
    'fpl2cc/',
    'fpl2cc/test/',
    'compiler/',
]
for sdir in src_subdirs: 
    SConscript('src/' + sdir + 'SConstruct', exports='env', variant_dir='build/'+sdir)

