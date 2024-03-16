(defpackage #:org.shirakumo.framebuffers.win32.cffi
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on))
  (:export
   #:user32
   #:gdi32
   #:shcore
   #:key
   #:minmax-info
   #:minmax-info-reserved-x
   #:minmax-info-reserved-y
   #:minmax-info-max-size-x
   #:minmax-info-max-size-y
   #:minmax-info-max-position-x
   #:minmax-info-max-position-y
   #:minmax-info-min-track-size-x
   #:minmax-info-min-track-size-y
   #:minmax-info-max-track-size-x
   #:minmax-info-max-track-size-y
   #:window-class
   #:window-class-style
   #:window-class-proc
   #:window-class-class-extra
   #:window-class-window-extra
   #:window-class-instance
   #:window-class-icon
   #:window-class-cursor
   #:window-class-background
   #:window-class-menu-name
   #:window-class-class-name
   #:message
   #:message-window
   #:message-type
   #:message-wparameter
   #:message-lparameter
   #:message-time
   #:message-x
   #:message-y
   #:bitmap-info
   #:bitmap-info-size
   #:bitmap-info-width
   #:bitmap-info-height
   #:bitmap-info-planes
   #:bitmap-info-bit-count
   #:bitmap-info-compression
   #:bitmap-info-size-image
   #:bitmap-info-x-per-meter
   #:bitmap-info-y-per-meter
   #:bitmap-info-clear-used
   #:bitmap-info-clear-important
   #:bitmap-info-red-mask
   #:bitmap-info-green-mask
   #:bitmap-info-blue-mask
   #:bitmap-info-alpha-mask
   #:rect
   #:rect-left
   #:rect-top
   #:rect-right
   #:rect-bottom
   #:track-mouse-event
   #:track-mouse-event-size
   #:track-mouse-event-flags
   #:track-mouse-event-track
   #:track-mouse-event-hover-time
   #:adjust-window-rect
   #:bit-blt
   #:change-display-settings
   #:create-window
   #:def-window-proc
   #:destroy-window
   #:dispatch-message
   #:enable-non-client-dpi-scaling
   #:enum-display-settings
   #:flash-window
   #:get-class-info
   #:get-dc
   #:get-device-caps
   #:get-dpi-for-monitor
   #:get-dpi-for-window
   #:get-message-time
   #:get-module-handle
   #:get-key-state
   #:get-system-metrics
   #:get-window
   #:invalidate-rect
   #:load-cursor
   #:load-cursor*
   #:map-virtual-key
   #:monitor-from-window
   #:msg-wait-for-multiple-objects
   #:peek-message
   #:register-class
   #:unregister-class
   #:release-dc
   #:send-message
   #:set-process-dpi-aware
   #:set-process-dpi-awareness-context
   #:set-process-dpi-awareness
   #:set-window
   #:set-window-pos
   #:set-window-text
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
   (#:win32 #:org.shirakumo.framebuffers.win32.cffi)
   (#:com #:org.shirakumo.com-on))
  (:export
   #:window))
