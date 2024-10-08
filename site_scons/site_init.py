import asyncio
import esml
import functools
import io
import inspect
import os
import pathlib
import pprint
import shlex
import signal
import subprocess
import subprof
import sys
import warnings

@functools.cache
def git_branch():
    branch = ''
    try:
        git_result = subprocess.run(
            [ 'git', 'rev-parse', '--abbrev-ref', 'HEAD' ],
            stdout=subprocess.PIPE
        )
        branch = git_result.stdout.decode('ascii').rstrip()
    except Exception as ex:
        warnings.warn(ex)
    return branch

def variant_dir(fn, variant):
    """
        Returns the "variant" filename (including path)
        for the filename passed.  This basically means
        returning the same path with the top level directory
        changed.
        If the filename passed is any false value, returns
        that value.  This allows pass-through when a filename
        is set to None (or similar).
        Scons also has a facility for doing this, but it
        only works with generated targets and has some
        peculiarities.  I want this for putting profiling
        data in a separate parallel tree, so here it is.
    """
    if not fn:
        return fn

    path = pathlib.Path(fn)
    parts = list(path.parts)
    if len(parts) > 1:
       parts[0] = variant
    # else there's no directory, so, I'm going to
    # say that the "variant" is just the filename;
    # i.e., there's nothing to vary, so there's no
    # variation from what's been passed.
    return os.path.join(*parts)

# In order to make profiling work, I'm going to monkeypatch
# os.waitpid() to call wait4() and store the results of that
# in a dictionary.  This seems to be the only (?) way to get
# the usage info about a child process at its exit (though,
# splitting hairs, this will also record resource usage on
# other child state changes).  It's not clear to me how to
# check if the child terminated or what from here.
#
# I'm not sure what this will do on Windows - hopefully
# wait4() will cleanly fail to exist, which we can detect,
# and just call the real_waitpid().
# Otherwise, this should be transparent to normal callers.
real_waitpid = os.waitpid
exit_results = { }
def waitpid_interceptor(pid, options, /):
    if callable(getattr(os, 'wait4')):
        result = os.wait4(pid, options)
        if result[0] > 0:
            exit_results[result[0]] = result
        return result[0], result[1]
    else:
        # wait4 isn't available, so just call the normal waitpid:
        return real_waitpid(pid, options)

os.waitpid = waitpid_interceptor

async def duplicate_output(from_stream, *outs):
    """
        Duplicates whatever it reads from the "from" file-like object
        into the other file-like objects passed.
    """
    done = False
    while not done:
        inp = await from_stream.read(1024) # (arbitrary buffer size)
        if len(inp) <= 0: # eof
            done = True
        else:
            for out in outs:
                out.write(inp)
                out.flush()

def run_command(command, env, timeout, quiet=False, profile=None):
    """
        Runs the command passed, in the env passed, duplicating
        stdout and stderr.

        Returns a dictionary containing:
            'returncode':  the exit code of the process
            'stderr': bytes object containing whatever was written to stderr
            'stdout': bytes object containing whatever was written to stdout

        The dictionary may also have:
            'exception': string describing any exceptions which occurred,
                         including timeouts
    """
    pout = io.BytesIO(b"")
    perr = io.BytesIO(b"")
    returncode = -255
    exception = None
    os_env = env.get('ENV', None);

    async def runit():
        nonlocal command, env, pout, perr, returncode, exception

        start_time = time.time()

        # print(' '.join(command) + f" {os_env}")
        proc = await asyncio.create_subprocess_exec(
            *command,
            stdin=sys.stdin,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            env=os_env
        )

        err_outputs = [ perr ]
        # we use stderr interactively, so unless we're told
        # otherwise, send it on directly as well as saving it:
        if not quiet:
            # stderr seems (on my system, anyway) to be opened in
            # "text" mode, while the subproc stderr seems to be in
            # binary mode, resulting in:
            #  "TypeError : write() argument must be str, not bytes"
            # So, to avoid that, we open a binary version of stderr
            # here and use that instead of the default stderr:
            stderr_b = os.fdopen(sys.stdout.fileno(), "wb", closefd=False)
            err_outputs.append(stderr_b)

        task = asyncio.gather(
            duplicate_output(proc.stdout, pout), 
            duplicate_output(proc.stderr, *err_outputs), 
            proc.wait()
        )

        try:
            await asyncio.wait_for(task, timeout=timeout)
            returncode = proc.returncode
        except asyncio.TimeoutError as t_o_ex:
            exception = (
                f"\nTimeout on {proc.pid} " +
                f"after {time.time() - start_time} seconds " +
                f"(max {timeout} secs).  Command: {command}\n" +
                f"(exception: {t_o_ex})\n"
            )
        except Exception as ex:
            exception = f"Exception thrown running {command}:\n{ex}"

        if exception is not None:
            proc.kill()
            warnings.warn(exception)

        # (this is what writes the profiling data to a tsv file)
        if profile:
            subprof.Profile(*exit_results[proc.pid]).write_tsv(profile)

    asyncio.run(runit())


    output = {
        'stderr': perr.getvalue(),
        'stdout': pout.getvalue(),
        'returncode': returncode
    }

    if exception is not None:
        output['exception'] = exception

    return output

def expand_scons_vars(instr, target, source, env):
    """
        Returns the string passed, but with scons $TARGET
        and $SOURCE variables resolved.
        If the object passed is not a string, it is returned
        unchanged.  (This allows booleans or None etc
        to be passed through and handled how the caller wants)
    """
    if isinstance(instr, str):
        return env.subst(instr, target=target, source=source)
    return instr

def default_kw(arg, env, kwargs, default=None):
    return env.get(arg, kwargs.get(arg, default))

def expand_overrides(arg, target, source, env, kwargs):
    return expand_scons_vars(default_kw(arg, env, kwargs), target, source, env)

def command_and_args(program, target, source, env):
    """
        Given a program list or string (potentially containing scons
        $TARGET, $SOURCE, etc, returns a list containing the command
        to run + arguments with $TARGET etc expanded.
    """
    # program should really be program + arguments (as a list),
    # but support passing a string:
    if isinstance(program, str):
        program = shlex.split(program)

    # substitute $TARGET etc into the command strings:
    return [expand_scons_vars(p, target, source, env) for p in program]

def run_and_capture_action(program, varlist=[], **kwargs):
    
    """
        Returns a scons Builder action which runs the source program
        specified, capturing stderr, stdout, and the exit value, and
        writing them as esml to a file.

        eg:

        env.Append(BUILDERS = {
            # compile a foo file to a bar file
            'CompileFoo' : Builder(
                action=run_and_capture_action("foo_compiler $SOURCE")
                suffix = '.bar'
                src_suffix = '.foo'
            )
        } )

        env.CompileFoo("xyz.bar", [ "xyz.foo" ], CAPFILE="xyz.out")

        Options:
            CAPFILE=<filename>  - If set, write stderr, stdout, and the
                                  program return code to the specified
                                  file (in esml format).  Expands scons
                                  $TARGET and $SOURCE variables in the
                                  specified filename.
            ENV=<assoc. array>  - If set, run with this os environment
            IGNORE_EXIT=<val>   - If set, ignore the exit value for purposes
                                  of determining if the action worked.
                                  The specific exit value is still captured
                                  and written to the capfile.
            INTERACTIVE=<val>   - If true, run interactively, passing stderr
                                  through and reading input from stdin
            PROFILE=<filename>  - If set to True, tells this to append
                                  profiling info to a file in the yprof tree
                                  corresponding to the target.
                                  If set to a string value, tells which file
                                  to append profiling info to.
                                  In both cases, the file (with path) is
                                  created if it doesn't already exist.
                                  Ignored if INTERACTIVE is true.  Defaults
                                  to None.
            STDERR=<filename>   - If set, write stderr to the specified file.
                                  Scons $TARGET and $SOURCE variables are
                                  expanded.
            STDOUT=<filename>   - If set, write stdout to the specified file.
                                  As with CAPFILE and STDERR, scons command
                                  line variables are expanded.
            TIMEOUT=<seconds>   - Kill the subproc if it's not done after
                                  this many seconds.  Ignored if INTERACTIVE
                                  is true; otherwise, defaults to 5.
            QUIET=<val>         - If true and we're not in INTERACTIVE mode,
                                  write stderr it to the capture file without
                                  echoing it to scons's stderr.

        Options may be passed in the Builder invocation, or to this function,
        or both.  Options in the invocation override any passed to this function.

        The generated esml target files contain objects with the following
        fields:
            'returncode': <exit value of the program>
            'stderr':     <whatever was written to stderr>
            'stdout':     <whatever was written to stdout>

    """

    def strfunction(target, source, env):
        return ' '.join(command_and_args(program, target, source, env))

    # This is the scons Builder action we return (i.e.,
    # the function scons calls when it wants to run the
    # action)
    def run_and_cap(target, source, env):
        nonlocal kwargs

        capfile = expand_overrides('CAPFILE', target, source, env, kwargs);
        errfile = expand_overrides('STDERR',  target, source, env, kwargs);
        outfile = expand_overrides('STDOUT',  target, source, env, kwargs);

        # remove the capture file first so that if something blows up
        # prior to generating the new one, we don't have a crufty one
        # lying aroung to confuse us:
        if capfile and os.path.isfile(capfile):
            os.remove(capfile)

        # substitute $TARGET etc into the command strings:
        command = command_and_args(program, target, source, env)

        # (for reporting)
        #strcommand = strfunction(target, source, env)
        #print(f"Going to run:\n    {strcommand}\n", file=sys.stderr);

        if env.get('INTERACTIVE', None):
            profile = None
            timeout = None
            quiet   = False
        else:
            profile = expand_overrides('PROFILE',  target, source, env, kwargs);
            if profile is True:
                profile = source[0].get_path() + '.tsv'

            # (profiling output goes in the yprof dir:)
            profile = variant_dir(profile, 'yprof')

            timeout = default_kw('TIMEOUT', env, kwargs, 5)
            quiet   = default_kw('QUIET', env, kwargs, False)

        output = run_command(
            command, env, timeout, quiet, profile
        )

        returncode = output['returncode']
        if (returncode is not None) and (returncode < 0):
            # process died of some signal (interrupt, segfault, whatever):
            try:
                signame = signal.Signals(-returncode).name
            except ValueError:
                signame = "(unknown signal)"
            strcommand = strfunction(target, source, env)
            msg = f"\n{signame} ({returncode}) failure on command \"{strcommand}\"\n"
            print(msg, file=sys.stderr)


        if capfile:
            with open(capfile, mode='wb') as outf:
                outf.write(bytes(
                    esml.dumps(output), encoding='utf-8', errors='ignore'
                ))

        if errfile:
            with open(errfile, mode='wb') as outf:
                outf.write(output['stderr'])

        if outfile:
            with open(outfile, mode='wb') as outf:
                outf.write(output['stdout'])

        if default_kw('IGNORE_EXIT', env, kwargs, False):
            # ignore "real" exit codes (which came from the program),
            # but do consider it failed if the program was killed by
            # a signal (by falling through)
            if (returncode is not None) and (returncode >= 0):
                return 0;

        return returncode

    # It appears we need to wrap this in an action so that when options
    # like "interactive" change, scons can know to rerun stuff.
    # https://scons.org/doc/production/HTML/scons-man.html
    #  ^ the key thing is the "build signature".
    # https://scons.org/doc/production/HTML/scons-man.html#action_objects
    # Possibly we didn't need or want to use all these closures.
    # errf ohwell
    return Action(
        run_and_cap,
        strfunction=strfunction,
        # varlist is needed, because otherwise scons
        # doesn't understand when something needs to
        # change due to changes in the environment:
        varlist=varlist + ['CAPFILE', 'INTERACTIVE']
    )

# Returns a scons Emitter which causes targets to depend
# on the thing passed.  Used (for example) to make files
# generated by fpl2cc depend on fpl2cc so that they get
# updated automatically when fpl2cc is updated.
def depend_on(primary):
    def thing_emitter(target, source, env):
        nonlocal primary
        for trg in target :
            #print(f"{trg} will depend on {primary}", file=sys.stderr)
            Depends(trg, primary)
        return target, source
        
    return thing_emitter

def depend_on_fpl2cc() :
    return depend_on('#bin/fpl2cc')

# We use variant_dir so as not to clutter up the source dirs
# with generated files, and we use duplicate=False so as not to
# clutter up the build dir with source files.  This is good
# and well, but it means that (for example) data files for
# tests (or whatever) stay in the source tree, which is also
# good and well but I can't find a way to get scons to tell me
# the REAL source path for getting at (eg) checked in data files.
# Hence this hack.
# source_in_variant_dir is a scons Node 
def source_dir(source_in_variant_dir) :
    components = source_in_variant_dir.get_tpath().split('/')
    components[0] = 'src';
    return Entry('#'+'/'.join(components[:-1]));
    

# Common fpl compile command line.
# Can be used to construct actions.
def fpl_compile_command():
    # Support running fpl-generated languages in debug mode.
    # Example usage:
    #   FPL_TEST_DEBUG=1 scons build/fpl2cc/test/wcalc.test/prec.success
    additional_options=""
    if 'FPL_TEST_DEBUG' in os.environ:
        additional_options="--debug-single-step"

    return f'bin/fpl2cc --src-path=src/grammarlib $FPLOPTS {additional_options} $SOURCES --out $TARGET --depfile .deps --statedump .states'


# Runs .cc-based tests for each file matching "*.expect".
# At the moment, if there are any .expect files which 
# don't correspond to .cc based programs, this will probably
# break.
# But, this serves my purposes at this moment.
def run_cc_tests(env):
    for scons_expect in Glob('*.expect'):
        # (tprog is a scons Node, not a python thingo)
        tprog, ext = os.path.splitext(scons_expect.name)
        main_src = tprog + '.cc'
        env.Program(tprog, [ main_src ])
    
        output_file = tprog + '.out'

        base_dir = str(Dir('#'))
        env['ENV']['BASE_DIR'] = base_dir
    
        # tests may want to find themselves and/or data directories:
        src_dir = source_dir(scons_expect).get_abspath();
        env['ENV']['SRC_DIR'] = os.path.relpath(src_dir, base_dir);
        #print(f"    source is {env['ENV']['SRC_DIR']} from {os.getcwd()}", file=sys.stderr)
        fp_data_dir = src_dir + '/' + tprog + '.data' # (full path)
        if(os.path.isdir(fp_data_dir)):
            #print(f"    DATA DIR {fp_data_dir} is a dir", file=sys.stderr)
            env['ENV']['DATA_DIR'] = os.path.relpath(fp_data_dir, base_dir)
            #print(f"    ... so it's {env['ENV']['DATA_DIR']} from {os.getcwd()}", file=sys.stderr)
    
            # output depends on everything in the data dir:
            for root, dirnames, filenames in os.walk(fp_data_dir):
                for fn in filenames:
                    env.Depends(output_file, f"{root}/{fn}")
    
        # in some cases tests use themselves as source data, and,
        # in any case, I'm going to say we want to regenerate test
        # output if the test source changes, so make that explicit
        # here:
        env.Depends(output_file, main_src)

        # Run the test, dumping stderr, stdout etc to the output
        # file.  We ignore the exit value for purposes of determining
        # test success, but (of course) we do check the return code
        # vs the one in the .expect file, so it's not actually ignored.
        env.RunAndCapture(
            output_file, tprog, IGNORE_EXIT=True
        )
    
        # compare the output of the test to the expected output;
        # .success file is/becomes up to date if output matched
        env.CompareOut(tprog + '.success', [ tprog + '.out', tprog + '.expect' ])

# split commands on the pipe symbol ('|') and
# return them as a list-of-lists
def _pipe_subcommands(cmd_and_args):
    if isinstance(cmd_and_args, str):
        cmd_and_args = shlex.split(cmd_and_args)

    cmds = [ [ ] ]
    for pi in range(len(cmd_and_args)):
        if cmd_and_args[pi] == '|':
            cmds.append([ ])
        else:
            cmds[-1].append(cmd_and_args[pi])

    return cmds

# Returns the stdout output of the command passed.
# For portablility, commands are not run through the
# default shell;  however, command output may be piped
# to further commands in the list using the '|' symbol.
# Also, $SHELL_VARIABLES are passed through as they are,
# which allows them to be evaluated when actual commands
# are run.
def conf_command(cmd_and_args, default=None):
    # We want to be able to support shell-style pipes
    # so that callers can filter command output.
    # so, find the pipe symbols:
    cmds = _pipe_subcommands(cmd_and_args);
    last_out = None
    try:
        for cmd in cmds:
            #print(f"command {cmd}", file=sys.stderr)
            if last_out:
                #print(f"   piping to {cmd}", file=sys.stderr)
                last_out = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=last_out.stdout)
            else:
                #print(f"   ... saving the pipe", file=sys.stderr)
                last_out = subprocess.Popen(cmd, stdout=subprocess.PIPE)
        # the "decode" here is, of course, making a lot of assumptions,
        # but is effectively necessary to make the output work in the
        # larger string-assuming context.  sigh.
        return last_out.stdout.read().rstrip().decode('utf-8')
    except FileNotFoundError:
        # "file not found" is what's tossed on a missing command.
        # we want this to not be an error, so that callers can
        # attempt a command like "llvm-config" and just default
        # if llvm isn't installed or is unavailable
        caller = inspect.getframeinfo(inspect.currentframe())
        print(f"defaulting conf for command {cmd_and_args} at {caller.filename}:{caller.lineno}")

    return default

# Functions for testing fpl-based languages:

# given a file name name like foo/bar/bat.test
# or bat.fpl, infers the language name "bat".
def language_from_basename(directory):
    langbase, ext = os.path.splitext(directory)
    return os.path.basename(langbase)

# this sets up:
#   [lang]: [lang].cc
#   [lang].cc: [lang].fpl #bin/fpl2cc
# for the language specified, and makes the builder for:
#   [x].result: [x].[lang] [lang]
def make_language_test_rules(env, directory, language):
    langbase = directory + '/' + language

    # (langprog = the name of the language executable)
    langprog = langbase

    # running anything written in the given language requires
    # that language to exist, so we want this dependency:
    env.Append(
        SCANNERS = Scanner(
            function = lambda node, env, path: [ langprog ],
            skeys = [ language ]
        )
    )

    # .. and now create a builder for running the language program:
    env.Append(BUILDERS = {
        'FPLTestLang_' + language: Builder(
            action = run_and_capture_action([langprog, "$SOURCE"]),
            emitter = depend_on(langprog),
            src_suffix = language,
            suffix = '.result'
        )
    })

    # .. and, the language program is built from <lang>.fpl
    langsource = langbase + '.fpl'
    langcc     = langbase + '.cc'
    capfile    = langbase + '.fpl_result'
    #print(f"fpl options on {langsource}: {fpl_options}", file=sys.stderr)
    env.CaptureFplCompile(
        [ langcc, capfile ],  [ langsource ],
        CAPFILE=capfile,
    )

    compiler = env.Program(langprog, [ langprog + '.cc' ])

#
# runs tests for languages generated by fpl.
#
# files involved in such tests are:
#
# dependencies:
#   [src language].test/[src language]: [src language].cc
#   [src language].test/[src language].cc: [src language].fpl
#   [src language].test/[test name].result: [src language].test/[test name].[src language] [src language]
#   [src language].test/[test name].success: [src language].test/[test name].expect [src language].test/[test name].result
#
# So each test language is defined by an .fpl file,
# and then each test consists of a source file for that language and
# a set of .expect files with corresponding expected .result files.
#
# This lets us black-box test the fpl compiler for various cases.
#
# references (these were hard to find):
#   scons' idea of a target:
#     https://scons.org/doc/latest/HTML/scons-api/SCons.Node/#
#   action variables:
#     https://scons.org/doc/production/HTML/scons-user.html#app-variables
#
def run_language_tests(env, test_dirs, profile=False):
    # sets up:
    #   [lang].test/[t].success: [lang].test/[t].expect [lang].test/[t].result
    # and the invocation of the generated language program:
    #   [x].result: [lang] [x].[lang]
    # for each "expect" file.
    for test_dir in test_dirs:
        # (Note: test_dir.get_abspath() gets the target path, not the source)
        absdir = test_dir.get_abspath()
        language = language_from_basename(absdir)
        make_language_test_rules(env, absdir, language)

        #print(f"test dir relpath is {test_dir.relpath}", file=sys.stderr)
        #print(f"test dir name    is {test_dir.name}", file=sys.stderr)

        # we might expect certain output from the fpl compile
        # itself (for example, to test warnings emitted by
        # fpl2cc):
        fpl_expecteds = Glob(test_dir.name + '/*.fpl_expect')
        for expected in fpl_expecteds:
            test_name, foo = os.path.splitext(expected.get_path())
            env.CompareOut(
                test_name + '.success', [ expected, test_name + '.fpl_result' ]
            )

        expecteds = Glob(test_dir.name + '/*.expect')
        for expected in expecteds:
            test_name, foo = os.path.splitext(expected.get_path())

            #print(f"test name is {test_name}", file=sys.stderr)
            src_file = test_name + '.' + language
            success_file = test_name + '.success'
            result_file  = test_name + '.result'
            # call the test lang compile rule for the soure:
            test_lang_builder = getattr(env, 'FPLTestLang_' + language)
            test_lang_builder(
                result_file, [ src_file ],
                CAPFILE="$TARGET",
                INTERACTIVE='FPL_TEST_DEBUG' in os.environ, # debug -> interactive
                IGNORE_EXIT=True, # so we can test failure cases
                PROFILE=profile or env.use_yprof,
                QUIET=True,  # avoid spam when testing failures
            )
            env.CompareOut(success_file, [ expected, result_file ])

# wrapper for the above to run it in the current dir
def run_target_language_tests(env):
    # (Note this is the scons Glob, so it returns target-path dirs)
    test_dirs = Glob('*.test')
    #print(f"\n\nRUNNING LANGUAGE TESTS: {test_dirs}\n\n");

    if len(test_dirs) <= 0:
        warnings.warn(f"No tests found in {Dir('.').srcnode().path}")
    else:
        run_language_tests(env, test_dirs)




