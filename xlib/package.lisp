(defpackage #:org.shirakumo.framebuffers.xlib.cffi
  (:use #:cl)
  (:export
   ))

(defpackage #:org.shirakumo.framebuffers.xlib
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:xlib #:org.shirakumo.framebuffers.xlib.cffi))
  (:export
   #:window))
