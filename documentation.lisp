#|
 This file is a part of simple-inferiors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-inferiors)

(defun checkdocs (&optional (package *package*))
  "Check that all functions, classes, and variables have docstrings."
  (do-symbols (symb package)
    (when (eql (symbol-package symb) package)
      (when (and (fboundp symb) (not (documentation symb 'function)))
        (warn "No documentation for function ~s." symb))
      (when (and (boundp symb) (not (documentation symb 'variable)))
        (warn "No documentation for variable ~s." symb))
      (when (and (find-class symb NIL) (not (documentation symb 'type)))
        (warn "No documentation for class ~s." symb)))))

(defmacro setdocs (&body pairs)
  "Easily set the documentation."
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  ((*cwd* variable)
   "The variable containing the current directory, virtually.
This variable is only resolved once needed, such as when using WITH-EXCHDIR.

See WITH-CHDIR
See WITH-EXCHDIR")

  ((invalid-location-error type)
   "Signalled if an attempt is made to change directory to a location that does not exist.

See LOCATION")

  (location
   "Attempts to resolve the THING to a pathname.

THING can be one of
  NULL     => UIOP:GETCWD
  PATHNAME => THING
  STRING   => UIOP:PARSE-NATIVE-NAMESTRING

This generic function is intended to be extended with methods by the user to allow using objects as locations directly.")

  (valid-location-p
   "Checks whether THING is a valid (existing) location.

See LOCATION")

  (check-location
   "Checks whether THIGN is a valid location and if it isn't, signals an INVALID-LOCATION-ERROR.

See VALID-LOCATION-P
See INVALID-LOCATION-ERROR")

  ((with-chdir)
   "Changes the directory lazily.
This merges the passed NEW-PATH (resolved through LOCATION) with *CWD* (resolved through LOCATION) and binds that to *CWD*.

See LOCATION
See *CWD*")

  ((with-exchdir)
   "Changes the directory directly.
If NEW-PATH is not passed, *CWD* is used instead. Either way it is resolved through LOCATION and checked by CHECK-LOCATION before the actual directory change is performed. This will /also/ act like WITH-CHDIR by additionally rebinding *CWD*.

Note that since a binary can only ever be in one directory at once, you should avoid using this unless necessary, or unless you are sure that the system is not paralellised.

See LOCATION
See CHECK-LOCATION
See *CWD*")

  ((with-resolved-stream)
   "Resolves the STREAM-ISH to an actual stream and rebinds the symbol.

See CALL-WITH-RESOLVED-STREAM.")

  (call-with-resolved-stream
   "Resolved STREAM-ISH to an actual stream and calls FUNC with it as its only argument.

STREAM can be one of
  NULL          => Uses an empty broadcast-stream.
  STREAM        => Uses the stream directly.
  PATHNAME      => OPENs the file (passing ARGS) and uses the obtained file-stream.
  (EQL :STRING) => Uses a string-output-stream.
  (EQL T)       => Uses *standard-output*")

  (copy-stream
   "Copies data from INPUT to OUTPUT using the given BUFFER format.
If CONSUME-ALL is non-NIL, all data is read from INPUT until EOF is reached.

BUFFER can be one of
  (EQL :LINE)      => The stream is copied one line at a time.
  (EQL :CAHRACTER) => The stream is copied one character at a time.
  INTEGER          => A character buffer of size BUFFER is used.

Note that this function tries to be as non-blocking as possible if CONSUME-ALL is NIL.
This means that it will only copy anything if there is something to read, but might
also read more than one line, character, or buffer at a time, if more data is available.

Once nothing more can be copied, FINISH-OUTPUT on OUTPUT is called.")

  (stop-process
   "Attempt to stop the PROCESS.
It will first try to send a SIGINT every SLEEP seconds for ATTEMPTS times.
If the process is still running at that point, a SIGKILL is sent. If the process
still won't quite after a SIGKILL, STOP-PROCESS simply gives up.")

  (ensure-process-stopped
   "Only tries to stop the PROCESS if it is still running.

See STOP-PROCESS")

  (handle-process-sequential
   "Handles the PROCESS using COPIER with the OUT-IN, OUT-OUT, ERR-IN, and ERR-OUT streams sequentially.
Between copies, it will sleep for COOLDOWN seconds to make sure no excessive CPU is wasted trying to read repeatedly.

As all handlers, this is responsible for copying the data from the IN to the respective OUT streams as well as ensuring that the process is stopped and all remaining data is read on unwinding.")

  (handle-process-parallel
   "Handles the PROCESS using COPIER with the OUT-IN, OUT-OUT, ERR-IN, and ERR-OUT streams in parallel. For that, it opens two threads for the respective stream pairs that handle the copying and joins them with the initial thread on unwinding.

As all handlers, this is responsible for copying the data from the IN to the respective OUT streams as well as ensuring that the process is stopped and all remaining data is read on unwinding.")

  (make-copier
   "Creates a copier function that accepts an input and output stream as well as optional extra arguments using BUFFER.
This simply creates a wrapper lambda around COPY-STREAM.

See COPY-STREAM")

  (ensure-copier
   "Ensures that COPIER-ISH is an actual function usable for copying streams.

COPIER-ISH can be one of
  FUNCTION => The function is used directly.
  INTEGER  => MAKE-COPIER is called with the COPIER-ISH.
  KEYWORD  => MAKE-COPIER is called with the COPIER-ISH.
  SYMBOL   => The function associated with the symbol is used.

The function must accept an INPUT and OUTPUT stream, as well as in the very least a keyword argument called CONSUME-ALL that, when non-NIL, will copy the whole INPUT to OUTPUT in one go until EOF is reached.

See MAKE-COPIER")

  ((*process-start-lock* variable)
   "Uses non non-SBCL implementations to lock the starting of the process and thus ensure there are no race conditions with switching the directory temporarily.")

  (%start-process
   "Minimal wrapper to EXTERNAL-PROGRAM:START for implementation specific corrections.

On SBCL uses the :DIRECTORY extra argument.
On non-SBCL uses the *PROCESS-START-LOCK* to lock the process, followed by WITH-EXCHDIR before starting the process.")

  ((inferior-process-failed-condition type)
   "Condition used for when a process returns with a non-zero exit code.

See FAILED-PROGRAM
See FAILED-ARGS
See FAILED-EXIT")

  (failed-program
   "Accesses the program string passed to RUN that failed to execute properly.

See INFERIOR-PROCESS-FAILED-CONDITION")

  (failed-args
   "Accesses the program arguments passed to RUN that failed to execute properly.

See INFERIOR-PROCESS-FAILED-CONDITION")

  (failed-exit
   "Accesses the exit code returned from the program that failed to execute properly in RUN.

See INFERIOR-PROCESS-FAILED-CONDITION")

  ((inferior-process-failed-error type)
   "Error variant of INFERIOR-PROCESS-FAILED-CONDITION

See INFERIOR-PROCESS-FAILED-CONDITION")

  ((inferior-process-failed-warning type)
   "Warning variant of INFERIOR-PROCESS-FAILED-CONDITION

See INFERIOR-PROCESS-FAILED-CONDITION")

  (run
   "Runs an inferior process, supplying PROGRAM with ARGS and using INPUT for STDIN, OUTPUT for STDOUT, and ERROR FOR STDERR.

The current *CWD* is resolved to an actual location, checked for validity, and then used as the location to start the process in. Depending on implementation support, this may have to fall back on using WITH-EXCHDIR for launching the process.

HANDLER must be a function of six arguments:
  COPIER  => The function computed by ENSURE-COPIER on COPIER:
  PROCESS => The process object used by EXTERNAL-PROGRAM.
  OUT-IN  => The receiving STDOUT stream from the process.
  OUT-OUT => The outputting stream computed by WITH-RESOLVED-STREAM on OUTPUT.
  ERR-IN  => The receiving STDERR stream from the process.
  ERR-OUT => The outputting stream computed by WITH-RESOLVED-STREAM on ERROR.
The handler must ensure that the process is stopped and all data has been copied when an unwind takes place. Furthermore it should not return until the process is done.

ON-NON-ZERO-EXIT can be one of
  NIL     => NIL is returned.
  :RETURN => The exit code is returned.
  :ERROR  => A INFERIOR-PROCESS-FAILED-ERROR is signalled.
  :WARN   => A INFERIOR-PROCESS-FAILED-WARNING is signalled.

See *CWD*
See WITH-EXCHDIR
See ENSURE-COPIER
See HANDLE-PROCESS-SEQUENTIAL
See HANDLE-PROCESS-PARALLEL"))
