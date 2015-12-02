## About Simple-Inferiors
This is a library to allow easy handling of external processes, and primarily to get their output. It handles proper copying of stdout and stderr of the process simultaneously, both in a sequential and parallel fashion. It also features a lazy directory switching mechanism, to avoid running into parallelism problems when having to `chdir`.

## How To
Load simple-inferiors with ASDF or Quicklisp

    (ql:quickload :simple-inferiors)

Run a program!

    (simple-inferiors:run "bash" '("-c" "for i in `seq 1 10`; do echo $i; done"))

Not very exciting. By default the output is discarded and you only get the exit code. Let's see what it says:

    (simple-inferiors:run "bash" '("-c" "for i in `seq 1 10`; do echo $i; done")
                          :output T)

By default the streams will be copied character by character. This allows the most immediate fetching of the output from the process, at the cost of being very CPU intensive. If you can afford it, you may want to switch to a more efficient method, such as copying line by line:

    (simple-inferiors:run "bash" '("-c" "for i in `seq 1 10`; do echo $i; done")
                          :output T :copier :line)

The output actually doesn't change for this tiny test case, but the performance can be radically different for larger outputs. You may also pass a number to specify a custom buffer size, or a function to handle the stream copying yourself.

When the stack is unwound, simple-inferiors tries to terminate the external process. By default it will send ten SIGINTs with 0.1 second delays and then a SIGKILL if it still hasn't terminated. In order to control this stopping, you must supply a different handler to `run`.

    (simple-inferiors:run "bash" '("-c" "sleep 60") 
                          :handler (lambda (c p oi oo ei eo) 
                                     (simple-inferiors:handle-process-sequential c p oi oo ei eo :stop-attempts 100)))

You may also use `handle-process-parallel` if you would like to use threads to handle the stdout and stderr of the process instead of attempting to read both simultaneously sequentially, or provide your own handler function entirely.

If you need to handle different directories for your process, you can use `with-chdir`. Note that `with-chdir` does not actually perform a `chdir` and instead rebinds `*cwd*`. The `chdir` is only performed (if at all necessary) at the very last stage when a process is run. This avoids clashing if parallelism is involved. `with-chdir` merges the passed location (resolved by `location`) with the current `*cwd*`. If `*cwd*` is `NIL` (such as at the very beginning), the `cwd` is used to merge the path. If you need to absolutely definitely perform a `chdir`, you may use `with-exchdir`. Note that it will signal an `invalid-location-error` if changing directory to an inexistent location is attempted.
