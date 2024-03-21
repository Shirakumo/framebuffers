(defpackage #:org.shirakumo.framebuffers.cocoa.cffi
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:objc #:org.shirakumo.cocoas))
  (:shadow #:close)
  (:export
   #:point
   #:point-x
   #:point-y
   #:size
   #:size-w
   #:size-h
   #:rect
   #:rect-x
   #:rect-y
   #:rect-w
   #:rect-h
   #:nsapplication-shared-application
   #:nsapp-set-activation-policy
   #:nsapp-activate-ignoring-other-apps
   #:nsapp-request-user-attention
   #:nsscreen-main-screen
   #:nspasteboard-general-pasteboard
   #:nscolor-clear-color
   #:frame
   #:make-rect
   #:make-size
   #:init
   #:init-with-content-rect
   #:alloc
   #:close
   #:is-zoomed
   #:convert-rect-to-backing
   #:content-rect-for-frame-rect
   #:set-release-when-closed
   #:set-opaque
   #:set-background-color
   #:set-accepts-mouse-moved-events
   #:set-title
   #:set-content-view
   #:make-first-responder-view
   #:cg-display-bounds
   #:cg-main-display-id
   #:set-frame
   #:frame-rect-for-content-rect
   #:set-frame-origin
   #:set-miniwindow-title
   #:order-front
   #:order-out
   #:zoom
   #:miniaturize
   #:deminiaturize
   #:set-content-min-size
   #:set-content-max-size
   #:make-key-and-order-front
   #:style-mask
   #:set-style-mask
   #:make-first-responder
   #:set-collection-behavior
   #:set-level
   #:types
   #:string-for-type
   #:declare-types
   #:set-string
   #:floating-window-level
   #:normal-window-level
   #:main-menu-window-level))

(defpackage #:org.shirakumo.framebuffers.cocoa
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:objc #:org.shirakumo.cocoas)
   (#:cocoa #:org.shirakumo.framebuffers.cocoa.cffi))
  (:export
   #:window))
