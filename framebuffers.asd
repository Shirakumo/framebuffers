(asdf:defsystem framebuffers
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A portable library for operating system framebuffers and windows, including IO handling"
  :homepage "https://shirakumo.github.io/framebuffers/"
  :bug-tracker "https://github.com/shirakumo/framebuffers/issues"
  :source-control (:git "https://github.com/shirakumo/framebuffers.git")
  :defsystem-depends-on (:trivial-features)
  :depends-on (:framebuffers/protocol
               (:feature (:and :unix (:not :darwin)) :framebuffers/xlib)
               (:feature :windows :framebuffers/win32)
               (:feature :linux :framebuffers/wayland)
               (:feature :darwin :framebuffers/cocoa)
               (:feature :mezzano :framebuffers/mezzano)))

(asdf:defsystem framebuffers/protocol
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "internal")
               (:file "linux" :if-feature :unix)
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-features
               :trivial-indent
               :nibbles
               (:feature (:not :mezzano) :static-vectors)
               (:feature (:not :mezzano) :cffi)))

(asdf:defsystem framebuffers/win32
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for Microsoft Windows' Win32"
  :serial T
  :components ((:module "win32"
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol
               :com-on))

(asdf:defsystem framebuffers/wayland
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for Linux' Wayland system"
  :serial T
  :components ((:module "wayland"
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol
               :framebuffers/xlib
               :mmap))

(asdf:defsystem framebuffers/xlib
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for the X11 server using Xlib"
  :serial T
  :components ((:module "xlib"
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol
               :mmap))

(asdf:defsystem framebuffers/cocoa
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for Apple's Cocoa platform"
  :serial T
  :components ((:module "cocoa"
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol
               :cffi-libffi
               :cocoas))

(asdf:defsystem framebuffers/mezzano
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for Mezzano"
  :serial T
  :components ((:module "mezzano"
                :components ((:file "package")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol))

#++
(asdf:defsystem framebuffers/drm
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Framebuffer backend for Linux' Direct Rendering Manager via libdrm"
  :serial T
  :components ((:module "cocoa"
                :components ((:file "package")
                             (:file "bindings")
                             (:file "keycodes")
                             (:file "wrapper"))))
  :depends-on (:framebuffers/protocol))

(asdf:defsystem framebuffers/test
  :components ((:module "test"
                :components ((:file "test"))))
  :depends-on (:framebuffers :parachute))
