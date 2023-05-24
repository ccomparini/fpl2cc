import asyncio
import esml
import io
import os
import pathlib
import pprint
import signal
import subprocess
import subprof
import sys
import warnings


# Returns the "variant" filename (including path)
# for the filename passed.  This basically means
# returning the same path with the top level directory
# changed.
# Scons also has a facility for doing this, but it
# only works with generated targets and has some
# peculiarities.  I want this for putting profiling
# data in a separate parallel tree, so here it is.
def variant_dir(fn, variant):
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
# other child state changes.  It's not clear to me how to
# check if the child terminated or what from here.
#
# I'm not sure what this will do on Windows - hopefully
# wait4() will cleanly fail to exist, which we can detect,
# and just call the real_waitpid()
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

def run_and_capture_action(program, varlist=[]):
    """
        Returns a scons Builder action which runs the source program
        specified, capturing stderr, stdout, and the exit value, and
        writing them as esml to a file.

        env.Append(BUILDERS = {
            # compile a foo file to a bar file
            'CompileFoo' : Builder(
                action=run_and_capture_action("foo_compiler $SOURCE")
                suffix = '.bar'
                src_suffix = '.foo'
            )
        } )

        env.CompileFoo("xyz.bar", [ "xyz.foo" ], CAPFILE="xyz.out")

        The following environment variables apply:
            CAPFILE=<filename>  - Tells where to put the captured output and
                                  exit value .  Default is the (scons) target.
            IGNORE_EXIT=<val>   - If set, ignore the exit value for purposes
                                  of determining if the action worked.
                                  The specific exit value is still captured
                                  and written to the capfile.
            INTERACTIVE=<val>   - If true, run interactively, passing stderr
                                  through and reading input from stdin
            PROFILE=<filename>  - Tells where to append profiling info.
                                  The file (with path) is created if it doesn't
                                  already exist.
                                  Ignored if INTERACTIVE is true.
            TIMEOUT=<seconds>   - Kill the subproc if it's not done after this
                                  many seconds.  Ignored if INTERACTIVE is
                                  true; otherwise, defaults to 5.
            QUIET=<val>         - If true and we're not in INTERACTIVE mode,
                                  write stderr it to the capture file without
                                  echoing it to scons's stderr.

        The generated esml target files contain objects with the following
        fields:
            'returncode': <exit value of the program>
            'stderr':     <whatever was written to stderr>
            'stdout':     <whatever was written to stdout>

    """


    # Duplicates whatever it reads from the "from" file-like object
    # into the other file-like objects passed.
    async def duplicate_output(from_stream, *outs):
        done = False
        while not done:
            inp = await from_stream.read(1024) # (arbitrary buffer size)
            if len(inp) <= 0: # eof
                done = True
            else:
                for out in outs:
                    out.write(inp)
                    out.flush()

    # Returns a tuple containing the exit code, and 2 bytes objects containing
    # whatever the subproc wrote to stdout and stderr (respectively).
    def run_command(command, env, timeout, quiet=False, profile=None):
        pout = io.BytesIO(b"")
        perr = io.BytesIO(b"")
        returncode = -255
        exception = None

        async def runit():
            nonlocal command, env, pout, perr, returncode

            proc = await asyncio.create_subprocess_exec(
                *command,
                stdin=sys.stdin,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
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
            except asyncio.TimeoutError:
                # subproc is taking too long.  kill it:
                warnings.warn(f"Timeout on {proc.pid} {command}\n")
                proc.kill()

            returncode = proc.returncode

            if profile:
                subprof.Profile(*exit_results[proc.pid]).write_tsv(profile)
                print(f"profile written to {profile}", file=sys.stderr)

        asyncio.run(runit())

        if exception is not None:
            raise exception

        return returncode, pout, perr


    # program should really be program + arguments, but support
    # just passing the name of the program:
    if isinstance(program, str):
        program = program.split()

    # returns the command to run + arguments, as a list, with
    # $TARGET etc expanded.
    def command_and_args(target, source, env):
        nonlocal program

        # substitute $TARGET etc into the command strings:
        return [env.subst(p, target=target, source=source) for p in program]

    def strfunction(target, source, env, executor=None):
        return ' '.join(command_and_args(target, source, env))

    # This is the scons Builder action we return:
    def run_and_cap(target, source, env):

        capfile = env.get('CAPFILE', None)

        # If we haven't been told the name of the file in which
        # to dump the capture, use the first target.  This is a
        # common use case in tests, where the main thing we care
        # about is the capture.
        if not capfile:
            capfile = target[0].get_path()

        # remove the capture file first so that if something blows up
        # prior to generating the new one, we don't have a crufty one
        # lying aroung to confuse us:
        if os.path.isfile(capfile):
            os.remove(capfile)

        # substitute $TARGET etc into the command strings:
        command = command_and_args(target, source, env)

        # (for reporting)
        strcommand = strfunction(target, source, env)
        #print(f"Going to run:\n    {strcommand}\n", file=sys.stderr);

        if env.get('INTERACTIVE', None):
            profile = None
            timeout = None
            quiet   = False
        else:
            profile = env.get('PROFILE', None)
            timeout = env.get('TIMEOUT', 5)
            quiet   = env.get('QUIET', False)

        returncode, pout, perr = run_command(
            command, env, timeout, quiet, profile
        )

        if (returncode is not None) and (returncode < 0):
            # process died of some signal (interrupt, segfault, whatever):
            try:
                signame = signal.Signals(-returncode).name
            except ValueError:
                signame = "(unknown signal)"
            msg = f"\n{signame} ({returncode}) failure on command \"{strcommand}\"\n"
            print(msg, file=sys.stderr)

        output = {
            'stderr': perr.getvalue(),
            'stdout': pout.getvalue()
        }
        if returncode is not None:
            output['returncode'] = returncode

        with open(capfile, mode='wb') as outf:
            outf.write(
                bytes(esml.dumps(output), encoding='utf-8', errors='ignore')
            )

        if env.get('IGNORE_EXIT', False):
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
    # return 'bin/fpl2cc --lr-stack-reserve=2 --param-stack-reserve=3 --src-path=src/grammarlib $FPLOPTS $SOURCES --out $TARGET --depfile .deps --statedump .states'
    return 'bin/fpl2cc --src-path=src/grammarlib $FPLOPTS $SOURCES --out $TARGET --depfile .deps --statedump .states'


