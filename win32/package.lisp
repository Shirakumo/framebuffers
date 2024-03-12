(defpackage #:org.shirakumo.framebuffers.win32.cffi
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on))
  (:export
   #:user32
   #:shcore
   #:rect
   #:rect-left
   #:rect-top
   #:rect-right
   #:rect-bottom
   #:adjust-window-rect
   #:bit-blt
   #:change-display-settings
   #:create-window
   #:def-window-proc
   #:destroy-window
   #:dispatch-message
   #:enable-non-client-dpi-scaling
   #:enum-display-settings
   #:get-dc
   #:get-dpi-for-monitor
   #:get-dpi-for-window
   #:get-message-time
   #:get-system-metrics
   #:get-window
   #:invalidate-rect
   #:peek-message
   #:register-class
   #:release-dc
   #:send-message
   #:set-process-dpi-aware
   #:set-process-dpi-awareness-context
   #:set-process-dpi-awareness
   #:set-window
   #:set-window-pos
   #:show-window
   #:stretch-di-bits
   #:translate-message
   #:validate-rect))

(defpackage #:org.shirakumo.framebuffers.win32
  (:use #:cl)
  (:shadow #:atom)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:win32 #:org.shirakumo.framebuffers.win32.cffi))
  (:export
   #:window))
