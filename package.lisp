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
   #:size
   #:location
   #:title
   #:visible-p
   #:maximized-p
   #:iconified-p
   #:clipboard-string
   #:content-scale
   #:buffer
   #:swap-buffers
   #:process-events
   #:request-attention
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
   #:file-dropped))

(defpackage #:org.shirakumo.framebuffers.int
  (:use #:cl #:org.shirakumo.framebuffers)
  (:shadowing-import-from #:org.shirakumo.framebuffers #:open #:close)
  (:export
   #:*available-backends*
   #:init-backend
   #:shutdown-backend
   #:open-backend
   #:default-title
   #:ptr-int
   #:ptr-window
   #:resize-buffer))
