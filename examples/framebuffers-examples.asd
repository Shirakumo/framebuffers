(asdf:defsystem framebuffers-examples
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Examples for the framebuffers system"
  :homepage "https://shirakumo.github.io/framebuffers/"
  :bug-tracker "https://github.com/shirakumo/framebuffers/issues"
  :source-control (:git "https://github.com/shirakumo/framebuffers.git")
  :depends-on (:framebuffers)
  :serial T
  :components ((:file "package")
               (:file "gradient")
               (:file "log-events")))
