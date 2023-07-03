(asdf:defsystem simple-inferiors
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
