import esml
import os
import subprocess # requires python 3.x
import threading

def run_and_capture_action(program, interactive=False):
    """
        Returns a scons Builder action which runs the source program
        specified, capturing stderr, stdout, and the exit value, and
        writes them as esml to the target file.

        env.Append(BUILDERS = {
            'CompileFoo' : Builder(
                action=run_and_capture_action("foo_compiler"),
                src_suffix = '.foo'
                suffix = '.esml'
            )
        } )

        The generated esml target files contain objects with the following
        fields:
            'returncode': <exit value of the program>
            'stderr':     <whatever was written to stderr>
            'stdout':     <whatever was written to stdout>

        The builder action returned 

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
            print("\ndone reading stderr\n", file=sys.stderr)

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
        program = [ program ]

    # This is the scons Builder action we return:
    def run_and_cap(target, source, env):
        if len(target) != 1:
            raise ValueError("run_and_capture must have exactly 1 target")

        nonlocal program, interactive

        # remove the target first so that if something blows up, we
        # don't have whatever the last run generated lying around:
        target_fn = target[0].get_path()
        if os.path.isfile(target_fn):
            os.remove(target_fn)

        command = program + [f.get_path() for f in source]

        proc = subprocess.Popen(command,
            stdin=sys.stdin, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )

        if interactive:
            pout, perr = run_interactively(proc)
        else:
            pout, perr = run_normally(proc)

        with open(target_fn, mode='wb') as outf:
            outf.write(bytes(esml.dumps( {
                    'returncode': proc.returncode,
                    'stderr':     perr,
                    'stdout':     pout
                }
            ), encoding='utf-8', errors='ignore'))

        return 0 # success if we got here without tossing an exception

    return run_and_cap;

