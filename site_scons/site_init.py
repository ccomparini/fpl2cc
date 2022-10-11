import esml
import os
import subprocess # requires python 3.x


def run_and_capture_action(program):
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

    """

    if isinstance(program, str):
        program = [ program ]

    def run_and_cap(target, source, env):
        if len(target) != 1:
            raise ValueError("run_and_capture must have exactly 1 target")

        nonlocal program

        # remove the target first so that if the progam fails, we
        # don't have whatever the last run generated lying around
        target_fn = target[0].abspath
        if os.path.isfile(target_fn):
            os.remove(target_fn)

        command = program + [f.abspath for f in source]
        cap = subprocess.run(command, capture_output=True, timeout=10)

        with open(target_fn, mode='w', encoding='utf-8') as outf:
            outf.write(esml.dumps( {
                    'returncode': cap.returncode,
                    'stderr':     cap.stderr.decode(),
                    'stdout':     cap.stdout.decode()
                }
            ))

        return 0 # success if we got here without tossing an exception

    return run_and_cap;

