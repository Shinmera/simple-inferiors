#|
 This file is a part of simple-inferiors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem simple-inferiors
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A very simple library to use inferior processes."
  :homepage "https://github.com/Shinmera/simple-inferiors"
  :serial T
  :components ((:file "package")
               (:file "process")
               (:file "documentation"))
  :depends-on (:uiop
               :external-program
               :bordeaux-threads))
