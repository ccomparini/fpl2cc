from pathlib import Path
import os
import subprocess
import sys

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '

fpl_include_dirs = [ 'src/grammarlib' ]

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
    FPLPATH = fpl_include_dirs,
    LIBPATH=[ '#lib' ],
    LIBS='jest_util',
    tools = [ 'default', 'clangxx', ],
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
# Otherwise, creates the target file(s) and returns 0 (= success)
def sources_are_same(target, source, env):
    last_contents = None
    for fn in source:
        with open(fn.abspath) as f: contents = f.readlines()
        if last_contents is not None:
            if contents != last_contents:
                sys.stderr.write("MISMATCH: {a} {b}\n".format(a=last_fn.path, b=fn.path))
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
fpl_args = '--src-path=' + ':'.join(fpl_include_dirs) + ' $FPLOPTS $SOURCES --out $TARGET --depfile .deps'
env.Append(BUILDERS =
    { 'Fpl2cc' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
	         suffix = '.cc',
	         src_suffix = '.fpl') } )

# fpl -> h builder:
env.Append(BUILDERS =
    { 'Fpl2h' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
	         suffix = '.h',
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

