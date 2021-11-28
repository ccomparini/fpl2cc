from pprint import pprint

#    PLATFORM=platform,
#    BINDIR="#export/foo/bin",
#    INCDIR=include,
#    LIBDIR=lib,
#    CPPPATH=[include],
#    LIBPATH=[lib],
#    LIBS='..whatever',
env = Environment(
    CCFLAGS = '-O2 -std=c++11 -Wno-parentheses',
    CPPPATH = [
        '.',
        '#src',
        '#src/util',
    ],
)

# fake "Scanner" to make it so that cc files generated
# by fpl files implicitly depend on fpl2cc:
def depend_on_fpl2cc(node, env, path) :
    # print(f'sub faking a depend for node: {node} env: {env} path: {path}')
    return [ '#bin/fpl2cc' ]

env.Append(
    SCANNERS = Scanner(
        function = depend_on_fpl2cc,
        skeys = ['.fpl']
    )
)

# fpl -> cc builder:
env.Append(BUILDERS =
    { 'Fpl2CC' : Builder(action = 'bin/fpl2cc $SOURCE --out $TARGET',
	         suffix = '.cc',
	         src_suffix = '.fpl') } )




SConscript('src/util/SConstruct', 'env');
SConscript('src/fpl2cc/SConstruct', 'env');
SConscript('src/compiler/SConstruct', 'env');

