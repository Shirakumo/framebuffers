(defpackage #:org.shirakumo.framebuffers.cocoa.cffi
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:objc #:org.shirakumo.cocoas))
  (:export))

(defpackage #:org.shirakumo.framebuffers.cocoa
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:objc #:org.shirakumo.cocoas)
   (#:cocoa #:org.shirakumo.framebuffers.cocoa.cffi))
  (:export
   #:window))
