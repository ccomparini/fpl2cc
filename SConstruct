# import pprint
from pathlib import Path
import subprocess
import sys

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '

fpl_include_dirs = [ 'src/fpl2cc/grammarlib' ]

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

# scanner for fpl interdependencies (i.e. so that
# scons can understand fpl imports):
def fpl_scan(node, env, arg):

    src = node.srcnode().get_path()
    path = ':'.join(env['FPLPATH'])

    # bootstrapping:  chuck an error telling people what to do if
    # there's no fpl2cc, which we need for determining dependencies
    if not Path('bin/fpl2cc').exists() :
        raise Exception("BOOTSTRAPPING: please run:\n    scons bin/fpl2cc\n")
        
    imports = subprocess.check_output(
        [ 'bin/fpl2cc', '--no-generate-code', '--dump-dependencies', '--src-path='+path, src ]
    ).decode('utf-8').splitlines()

    # import names are relative to the current directory,
    # so make scons aware of that by prefixing them with "#":
    imports = list(map(lambda fl: "#" + fl, imports))

    # also, we'll want to reprocess fpls if fpl2cc changed:
    imports.append('#bin/fpl2cc')

    return imports

fpl_scanner = Scanner(function = fpl_scan, skeys = ['.fpl'])

# fpl -> cc builder:
fpl_args = '--src-path=' + ':'.join(fpl_include_dirs) + ' $FPLOPTS $SOURCES --out $TARGET'
env.Append(BUILDERS =
    { 'Fpl2cc' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
	         suffix = '.cc',
                 source_scanner = fpl_scanner,
	         src_suffix = '.fpl') } )

# fpl -> h builder:
env.Append(BUILDERS =
    { 'Fpl2h' : Builder(action = debugger + 'bin/fpl2cc ' + fpl_args,
	         suffix = '.h',
                 source_scanner = fpl_scanner,
	         src_suffix = '.fpl') } )

# fpl -> jest builder:
env.Append(BUILDERS =
    { 'Fpl2jest' : Builder(action = debugger + 'bin/fpl ' + fpl_args,
	         suffix = '.jest',
                 source_scanner = fpl_scanner,
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

