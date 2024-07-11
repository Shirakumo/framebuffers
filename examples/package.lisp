(defpackage #:org.shirakumo.framebuffers.examples
  (:nicknames #:framebuffers-examples)
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int))
  (:export
   #:gradient
   #:log-events))
