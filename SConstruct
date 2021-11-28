from pprint import pprint

env = Environment(
#    PLATFORM=platform,
#    BINDIR="#export/foo/bin",
#    INCDIR=include,
#    LIBDIR=lib,
#    CPPPATH=[include],
#    LIBPATH=[lib],
#    LIBS='..whatever',
    # CPPPATH = [ 'src/compiler', 'src/util', ],
)

#env.Repository("#src")


#pprint(vars(env))

# .... seems like this needs to exist only (?) in the subdir.
# env not shared.  hmmm.
## fake "Scanner" to make it so that cc files generated
## by fpl files implicitly depend on fpl2cc:
#def depend_on_fpl2cc(node, env, path, arg) :
#    print(f'faking a depend for node: {node} env: {env} path: {path} arg: {arg}')
#    return [ 'bin/fpl2cc' ],
#
#env.Append(
#    SCANNERS = Scanner(
#        function = depend_on_fpl2cc,
#        skeys = ['.fpl']
#    )
#)
#
#env.Append(BUILDERS =
#    { 'Fpl2CC' : Builder(action = 'bin/fpl2cc $sources --out $target',
#	         suffix = '.cc',
#	         src_suffix = '.fpl') } )
#
#                      suffix='.cc', src_suffix='.fpl')})
#
#  from sample:
#env.Copy1('test.bar') # produces test.h from test.bar. 
#env.Program('app','main.cpp') # indirectly depends on test.bar

# SConscript('src/util/SConstruct', variant_dir='scb',  duplicate=0)
#SConscript('src/fpl2cc/SConstruct', variant_dir='#obj',  duplicate=False)
SConscript('src/fpl2cc/SConstruct');
SConscript('src/compiler/SConstruct');

