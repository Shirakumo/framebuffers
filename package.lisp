(defpackage #:org.shirakumo.framebuffers
  (:use #:cl)
  (:shadow #:open #:close)
  (:export
   #:framebuffer-error
   #:init
   #:shutdown
   #:window
   #:event-handler
   #:open
   #:with-window
   #:do-pixels
   #:valid-p
   #:close-requested-p
   #:close
   #:width
   #:height
   #:minimum-size
   #:maximum-size
   #:size
   #:location
   #:title
   #:visible-p
   #:maximized-p
   #:iconified-p
   #:focused-p
   #:borderless-p
   #:always-on-top-p
   #:resizable-p
   #:floating-p
   #:mouse-entered-p
   #:clipboard-string
   #:content-scale
   #:buffer
   #:swap-buffers
   #:process-events
   #:request-attention
   #:mouse-location
   #:mouse-button-pressed-p
   #:key-pressed-p
   #:key-scan-code
   #:local-key-string
   #:window-moved
   #:window-resized
   #:window-refreshed
   #:window-focused
   #:window-iconified
   #:window-maximized
   #:window-closed
   #:mouse-button-changed
   #:mouse-moved
   #:mouse-entered
   #:mouse-scrolled
   #:key-changed
   #:string-entered
   #:file-dropped
   #:content-scale-changed))

(defpackage #:org.shirakumo.framebuffers.int
  (:use #:cl #:org.shirakumo.framebuffers)
  (:shadowing-import-from #:org.shirakumo.framebuffers #:open #:close)
  (:export
   #:*available-backends*
   #:static-file
   #:init-backend
   #:shutdown-backend
   #:open-backend
   #:default-title
   #:ptr-int
   #:ptr-window
   #:list-windows
   #:resize-buffer
   #:with-cleanup))
