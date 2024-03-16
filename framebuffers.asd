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
               (:file "internal")
               (:file "documentation")
               (:module "xlib"
                :if-feature :unix
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper")))
               (:module "wayland"
                :if-feature :linux
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper")))
               (:module "win32"
                :if-feature :windows
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper")))
               #++
               (:module "cocoa"
                :if-feature :darwin
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper")))
               #++
               (:module "mezzano"
                :if-feature :mezzano
                :components ((:file "package")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:documentation-utils
               :trivial-features
               :trivial-indent
               (:feature (:not :mezzano) :static-vectors)
               (:feature (:not :mezzano) :cffi)
               (:feature :darwin :float-features)
               (:feature :windows :com-on)
               (:feature :unix :mmap)))

(asdf:defsystem framebuffers/test
  :components ((:module "test"
                :components ((:file "test"))))
  :depends-on (:framebuffers))
