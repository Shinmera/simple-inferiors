#|
 This file is a part of simple-inferiors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem simple-inferiors
  :version "1.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A very simple library to use inferior processes."
  :homepage "https://Shinmera.github.io/simple-inferiors/"
  :bug-tracker "https://github.com/Shinmera/simple-inferiors/issues"
  :source-control (:git "https://github.com/Shinmera/simple-inferiors.git")
  :serial T
  :components ((:file "package")
               (:file "process")
               (:file "documentation"))
  :depends-on (:uiop
               :bordeaux-threads
               :documentation-utils))
