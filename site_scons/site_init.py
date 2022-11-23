import esml
import os
import pprint
import signal
import subprocess # requires python 3.x
import threading

def run_and_capture_action(program, interactive=False):
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
    # Returns a tuple of bytes containing whatever was written to
    # (stdout, stderr).
    def run_interactively(proc):
        perr = b""
        stderr_done = False
        def read_stderr():
            nonlocal proc, perr, stderr_done
            while not stderr_done:
                inp = proc.stderr.read(1)
                if len(inp) <= 0: # eof
                    # cmc note: on my OSX box I can't seem to get here.
                    stderr_done = True
                else:
                    # we use stderr interactively, so send it on directly
                    # as well as saving it:
                    sys.stderr.buffer.write(inp)
                    sys.stderr.flush()
                    perr += inp
            #print("\ndone reading stderr\n", file=sys.stderr)

        # From the docs (https://docs.python.org/3/library/threading.html):
        # "The significance of [the daemon flag] is that the entire Python
        # program exits when only daemon threads are left."  (Note that the
        # main thread isn't a daemon thread.)  Basically, it seems to mean
        # that we don't have to join() it, which we is important because
        # proc.stderr.read() seems to be blocking even when there's nothing
        # to read (?), which means the sub never ends, and join() never
        # returns.
        # It's ok if the thread doesn't terminate normally - we should
        # be able to read anything left over after the subproc terminates.
        stderr_rdr = threading.Thread(target=read_stderr, daemon=True)
        stderr_rdr.start()

        # No timeout if it's interactive - let the user take as long as
        # they want.
        proc.wait()

        # Finish reading anything we haven't yet read from stderr:
        inp = proc.stderr.read()
        if len(inp):
            perr += inp
            sys.stderr.buffer.write(inp)
            sys.stderr.flush()

        # Finally, deal with the proc's stdout:
        pout = proc.stdout.read()
        sys.stdout.buffer.write(pout)
        sys.stdout.flush()

        return pout, perr

    # runs the proc passed normally (blocking), and returns
    # a tuple with whatever was written to (stdout, stderr).
    def run_normally(proc, timeout=5):
        try:
            proc.wait(timeout)
        except subprocess.TimeoutExpired:
            proc.kill()
            raise
        return proc.stdout.read(), proc.stderr.read()

    # program should really be program + arguments, but support
    # just passing the name of the program:
    if isinstance(program, str):
        program = program.split()

    # This is the scons Builder action we return:
    def run_and_cap(target, source, env):

        nonlocal program, interactive

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
        command = [env.subst(p, target=target, source=source) for p in program]

        strcommand = ' '.join(command) # for error messages, etc.
        #print(f"\ncommand will be:\n   {strcommand}\n", file=sys.stderr)
        #print(f"\ncommand will be:\n   {strcommand}\n")
        proc = subprocess.Popen(command,
            stdin=sys.stdin, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )

        if interactive:
            pout, perr = run_interactively(proc)
        else:
            pout, perr = run_normally(proc)

        if(proc.returncode < 0):
             # process died of some singal (interrupt, segfault, whatever):
             signame = signal.Signals(-proc.returncode).name
             msg = f"\n{signame} failure on command \"{strcommand}\"\n"
             print(msg, file=sys.stderr)

        with open(capfile, mode='wb') as outf:
            outf.write(bytes(esml.dumps( {
                    'returncode': proc.returncode,
                    'stderr':     perr,
                    'stdout':     pout
                }
            ), encoding='utf-8', errors='ignore'))

        return 0 # success if we got here without tossing an exception

    return run_and_cap;

# Returns a scons Emitter which causes targets to depend
# on the thing passed.  Used (for example) to make files
# generated by fpl2cc depend on fpl2cc so that they get
# updated automatically when fpl2cc is updated.
def depend_on(primary):
    def thing_emitter(target, source, env):
        nonlocal primary
        # hey wait, should it depend on source or target?
        # the origin depend_on_fpl2cc made it depend on
        # source, and there was a comment saying taht was
        # deliberate.  deliberate, but was it correct?
        #for targ in target :
        #    print("")
        for src in source :
            Depends(src, primary)
        return target, source
        
    return thing_emitter

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

# Common fpl compile command line action.  Usable by scons Builders.
fpl_args = '--src-path=src/grammarlib' + ' $FPLOPTS $SOURCES --out $TARGET --depfile .deps --statedump .states'
def fpl_compile_action():
    return 'bin/fpl2cc ' + fpl_args


