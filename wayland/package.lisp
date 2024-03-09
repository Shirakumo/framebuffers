(defpackage #:org.shirakumo.framebuffers.wayland.cffi
  (:use #:cl)
  (:export
   #:wayland))

(defpackage #:org.shirakumo.framebuffers.wayland
  (:use #:cl)
  (:shadow #:atom)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:wl #:org.shirakumo.framebuffers.wayland.cffi))
  (:export
   #:window))
