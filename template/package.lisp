(defpackage #:org.shirakumo.framebuffers.BACKEND.cffi
  (:use #:cl)
  (:export))

(defpackage #:org.shirakumo.framebuffers.BACKEND
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:BACK #:org.shirakumo.framebuffers.BACKEND.cffi))
  (:export
   #:window))
