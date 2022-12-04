import asyncio
import esml
import os
import pprint
import signal
import subprocess

def run_and_capture_action(program, varlist=[]):
    """
        Returns a scons Builder action which runs the source program
        specified, capturing stderr, stdout, and the exit value, and
        writes them as esml to the target file.

        env.Append(BUILDERS = {
            # compile a foo file to a bar file
            'CompileFoo' : Builder(
                action=run_and_capture_action("foo_compiler $SOURCE")
                suffix = '.bar'
                src_suffix = '.foo'
            )
        } )

        env.CompileFoo("xyz.bar", [ "xyz.foo" ], CAPFILE="xyz.out")

        The generated esml target files contain objects with the following
        fields:
            'returncode': <exit value of the program>
            'stderr':     <whatever was written to stderr>
            'stdout':     <whatever was written to stdout>

    """

    # Run the proc passed "interactively", printing stderr and
    # stdout as well as buffering them.
    # Presently, stderr is the only stream which is "interactive"
    # (stdout actually just gets buffered and printed at the end),
    # because it so happens that the way tests work is that stdout
    # contains the output of the parser or whatever we're testing,
    # but stderr may contain (among other things) interactive
    # debugging info and prompts.
    # Returns a tuple of bytes objects containing whatever the subproc
    # wrote to stdout and stderr (respectively)
    def run_interactively(command):
        pout = b""
        perr = b""
        returncode = -255
        async def runit():
            nonlocal command, pout, perr, returncode
            proc = await asyncio.create_subprocess_exec(
                *command,
                stdin=sys.stdin,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            )
            async def tee_stderr():
                nonlocal proc, perr
                done = False
                while not done:
                    inp = await proc.stderr.read(256)
                    if len(inp) <= 0: # eof
                        done = True
                    else:
                        # we use stderr interactively, so send it on directly
                        # as well as saving it:
                        sys.stderr.buffer.write(inp)
                        sys.stderr.flush()
                        perr += inp

            async def read_stdout():
                nonlocal proc, pout
                done = False
                while not done:
                    inp = await proc.stdout.read(256) # 256 is an arbitrary buffer size
                    if len(inp) <= 0: # eof
                        done = True
                    else:
                        pout += inp

            await asyncio.gather(read_stdout(), tee_stderr(), proc.wait())
            returncode = proc.returncode

        asyncio.run(runit())
        return returncode, pout, perr

    # runs the proc passed normally (blocking), and returns
    # a tuple with whatever was written to (stdout, stderr).
    def run_normally(command, timeout=5):
        proc = subprocess.Popen(command,
            stdin=sys.stdin, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
        pout, perr = proc.communicate(timeout=timeout)
        return proc.returncode, pout, perr

    # program should really be program + arguments, but support
    # just passing the name of the program:
    if isinstance(program, str):
        program = program.split()

    # This is the scons Builder action we return:
    def run_and_cap(target, source, env):

        nonlocal program

        interactive = env.get('INTERACTIVE', None)
        capfile     = env.get('CAPFILE', None)

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
        command = [env.subst(p, target=target, source=source) for p in program]

        strcommand = ' '.join(command) # for error messages, etc.

        #print(f"Going to run:\n    {strcommand}\n", file=sys.stderr);

        if interactive:
            returncode, pout, perr = run_interactively(command)
        else:
            returncode, pout, perr = run_normally(command)

        if(returncode < 0):
             # process died of some signal (interrupt, segfault, whatever):
             signame = signal.Signals(-returncode).name
             msg = f"\n{signame} ({returncode}) failure on command \"{strcommand}\"\n"
             print(msg, file=sys.stderr)

        with open(capfile, mode='wb') as outf:
            outf.write(bytes(esml.dumps( {
                    'returncode': returncode,
                    'stderr':     perr,
                    'stdout':     pout
                }
            ), encoding='utf-8', errors='ignore'))

        return 0 # success if we got here without tossing an exception

    # It appears we need to wrap this in an action so that when options
    # like "interactive" change, scons can know to rerun stuff.
    # https://scons.org/doc/production/HTML/scons-man.html
    #  ^ the key thing is the "build signature".
    # https://scons.org/doc/production/HTML/scons-man.html#action_objects
    # Possibly we didn't need or want to use all these closures.
    # errf ohwell
    return Action(run_and_cap, varlist=varlist + ['CAPFILE', 'INTERACTIVE'])

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

# Common fpl compile command line.
# Can be used to construct actions.
def fpl_compile_command():
    return 'bin/fpl2cc --src-path=src/grammarlib $FPLOPTS $SOURCES --out $TARGET --depfile .deps --statedump .states'


