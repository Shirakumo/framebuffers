(asdf:defsystem framebuffers
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.github.io/framebuffers/"
  :bug-tracker "https://github.com/shirakumo/framebuffers/issues"
  :source-control (:git "https://github.com/shirakumo/framebuffers.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "common")
               (:file "documentation")
               (:module "xlib"
                :if-feature (:or :bsd :linux)
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper")))
               #++
               (:module "wayland"
                :if-feature :linux
                :components ((:file "package")))
               #++
               (:module "win32"
                :if-feature :windows
                :components ((:file "package")))
               #++
               (:module "cocoa"
                :if-feature :darwin
                :components ((:file "package"))))
  :depends-on (:documentation-utils
               :float-features
               :static-vectors
               :trivial-features
               :cffi))
