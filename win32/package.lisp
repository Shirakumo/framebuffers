(defpackage #:org.shirakumo.framebuffers.win32.cffi
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on))
  (:export))

(defpackage #:org.shirakumo.framebuffers.win32
  (:use #:cl)
  (:shadow #:atom)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:win32 #:org.shirakumo.framebuffers.win32.cffi))
  (:export
   #:window))
