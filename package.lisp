(defpackage #:org.shirakumo.framebuffers
  (:use #:cl)
  (:export
   #:init
   #:shutdown
   #:window
   #:open
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
   #:swap-buffers
   #:process-events
   #:request-attention
   #:window-moved
   #:window-resized
   #:window-refreshed
   #:window-focused
   #:window-iconified
   #:window-maximized
   #:mouse-button-changed
   #:mouse-moved
   #:mouse-entered
   #:mouse-scrolled
   #:key-changed
   #:string-entered
   #:file-dropped))

(defpackage #:org.shirakumo.framebuffers.int
  (:use #:cl)
  (:import-from
   #:org.shirakumo.framebuffers
   #:*available-backends*
   #:init-backend
   #:shutdown-backend
   #:open-backend
   #:default-title
   #:ptr-int
   #:ptr-window)
  (:export
   #:*available-backends*
   #:init-backend
   #:shutdown-backend
   #:open-backend
   #:default-title
   #:ptr-int
   #:ptr-window))
