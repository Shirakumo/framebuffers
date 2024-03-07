(defpackage #:org.shirakumo.framebuffers.xlib.cffi
  (:use #:cl)
  (:export
   #:x11
   #:xid
   #:atom
   #:image
   #:image-width
   #:image-height
   #:image-xoffset
   #:image-format
   #:image-data
   #:image-byte-order
   #:image-bitmap-unit
   #:image-bitmap-bit-order
   #:image-bitmap-pad
   #:image-depth
   #:image-bytes-per-line
   #:image-bits-per-pixel
   #:image-red-mask
   #:image-green-mask
   #:image-blue-mask
   #:image-obdata
   #:image-create-image
   #:image-destroy-image
   #:image-get-pixel
   #:image-put-pixel
   #:image-sub-image
   #:image-add-pixel
   #:set-window-attributes
   #:set-window-attributes-background-pixmap
   #:set-window-attributes-background-pixel
   #:set-window-attributes-border-pixmap
   #:set-window-attributes-border-pixel
   #:set-window-attributes-bit-gravity
   #:set-window-attributes-win-gravity
   #:set-window-attributes-backing-store
   #:set-window-attributes-backing-planes
   #:set-window-attributes-backing-pixel
   #:set-window-attributes-save-under
   #:set-window-attributes-event-mask
   #:set-window-attributes-do-not-progagate-mask
   #:set-window-attributes-override-redirect
   #:set-window-attributes-colormap
   #:set-window-attributes-cursor
   #:pixmap-format
   #:pixmap-format-depth
   #:pixmap-format-bits-per-pixel
   #:pixmap-format-scanline-pad
   #:size-hint
   #:size-hint-flags
   #:size-hint-x
   #:size-hint-y
   #:size-hint-width
   #:size-hint-height
   #:size-hint-min-width
   #:size-hint-min-height
   #:size-hint-max-width
   #:size-hint-max-height
   #:size-hint-width-inc
   #:size-hint-height-inc
   #:size-hint-min-aspect-x
   #:size-hint-min-aspect-y
   #:size-hint-max-aspect-x
   #:size-hint-max-aspect-y
   #:size-hint-base-width
   #:size-hint-base-height
   #:size-hint-win-gravity
   #:event
   #:base-event
   #:base-event-type
   #:base-event-serial
   #:base-event-send-event
   #:base-event-display
   #:base-event-window
   #:positioned-event
   #:positioned-event-root
   #:positioned-event-subwindow
   #:positioned-event-time
   #:positioned-event-x
   #:positioned-event-y
   #:positioned-event-x-root
   #:positioned-event-y-root
   #:key-event
   #:key-event-state
   #:key-event-keycode
   #:key-event-same-screen
   #:button-event
   #:button-event-state
   #:button-event-button
   #:button-event-same-screen
   #:focus-change-event
   #:focus-change-event-mode
   #:focus-change-event-detail
   #:configure-event
   #:configure-event-x
   #:configure-event-y
   #:configure-event-width
   #:configure-event-height
   #:configure-event-border-width
   #:configure-event-above
   #:configure-event-override-redirect
   #:property-event
   #:property-event-atom
   #:property-event-time
   #:property-event-state
   #:destroy-window-event
   #:client-message-event
   #:client-message-event-message-type
   #:client-message-event-format
   #:client-message-event-data
   #:net-message-event-protocol
   #:black-pixel
   #:default-depth
   #:default-gc
   #:default-root-window
   #:default-visual
   #:default-screen
   #:display-height
   #:display-width
   #:init-threads
   #:free-threads
   #:change-property
   #:clear-window
   #:clear-area
   #:close-display
   #:create-image
   #:create-window
   #:destroy-image
   #:destroy-window
   #:events-queued
   #:flush
   #:free
   #:intern-atom
   #:list-pixmap-formats
   #:lookup-string
   #:map-raised
   #:unmap-window
   #:next-event
   #:open-display
   #:peek-event
   #:pending
   #:put-image
   #:select-input
   #:set-wm-normal-hints
   #:set-wm-protocols
   #:store-name
   #:move-window
   #:resize-window
   #:send-event
   #:iconify-window
   #:xkb-keycode-to-keysym
   #:shm-segment-info
   #:shm-segment-info-message
   #:shm-segment-info-id
   #:shm-segment-indo-address
   #:shm-segment-info-read-only
   #:xshm-query-extension
   #:xshm-pixmap-format
   #:xshm-put-image
   #:xshm-create-image))

(defpackage #:org.shirakumo.framebuffers.xlib
  (:use #:cl)
  (:shadow #:atom)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:xlib #:org.shirakumo.framebuffers.xlib.cffi))
  (:export
   #:window))
