(in-package #:org.shirakumo.framebuffers.cocoa.cffi)

(cffi:defcstruct (point :conc-name point-)
  (x objc:cgfloat)
  (y objc:cgfloat))

(cffi:defcstruct (size :conc-name size-)
  (w objc:cgfloat)
  (h objc:cgfloat))

(cffi:defcstruct (rect :conc-name rect-)
  (x objc:cgfloat)
  (y objc:cgfloat)
  (w objc:cgfloat)
  (h objc:cgfloat))
