#|
 This file is a part of simple-inferiors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:simple-inferiors
  (:nicknames #:org.shirakumo.simple-inferiors)
  (:use #:cl)
  ;; process.lisp
  (:export
   #:*cwd*
   #:invalid-location-error
   #:location
   #:valid-location-p
   #:with-chdir
   #:with-exchdir
   #:copy-stream
   #:handle-process-sequential
   #:handle-process-parallel
   #:make-copier
   #:ensure-copier
   #:failed-program
   #:failed-args
   #:failed-exit
   #:inferior-process-failed-error
   #:inferior-process-failed-warning
   #:run))
