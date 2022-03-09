from pprint import pprint

# TODO you _can_ get the target files to not be slapped into
# the source tree!  Maybe?
# https://scons.org/doc/4.3.0/HTML/scons-user.html#chap-separate

debugger = ''
#debugger = 'TERM=xterm-256color /usr/bin/lldb --one-line "b debug_hook" -- '

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
    # print('sub faking a depend for node: {node} env: {env} path: {path}')
    # print('node ' + str(node) + ' depends on fpl2cc so it\'s probably fpl')
    return [ '#bin/fpl2cc' ]

env.Append(
    SCANNERS = Scanner(
        # this is wrong for Fpl2jest.  sigh.
        # can't I force the dependency in the builder?
        function = depend_on_fpl2cc,
        skeys = ['.fpl']
    )
)

# fpl -> cc builder:
# at this point this is for testing fpl
env.Append(BUILDERS =
    { 'Fpl2CC' : Builder(action = debugger + 'bin/fpl2cc $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.cc',
	         src_suffix = '.fpl') } )

# fpl -> h builder:
env.Append(BUILDERS =
    { 'Fpl2h' : Builder(action = debugger + 'bin/fpl2cc $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.h',
	         src_suffix = '.fpl') } )

# fpl -> jest builder:
env.Append(BUILDERS =
    { 'Fpl2jest' : Builder(action = debugger + 'bin/fpl $FPLOPTS $SOURCES --out $TARGET',
	         suffix = '.jest',
	         src_suffix = '.fpl') } )


SConscript('src/util/SConstruct', 'env');
SConscript('src/fpl2cc/SConstruct', 'env');
SConscript('src/compiler/SConstruct', 'env');

