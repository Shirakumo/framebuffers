(defpackage #:org.shirakumo.framebuffers.wayland.cffi
  (:use #:cl)
  (:export
   #:wayland
   #:marshal-flag-destroy
   #:message
   #:message-name
   #:message-signature
   #:message-types
   #:interface
   #:interface-name
   #:interface-version
   #:interface-method-count
   #:interface-methods
   #:interface-event-count
   #:interface-events
   #:argument
   #:event-queue-destroy
   #:proxy-marshal-flags
   #:proxy-marshal
   #:proxy-create
   #:proxy-create-wrapper
   #:proxy-wrapper-destroy
   #:proxy-marshal-constructor
   #:proxy-marshal-constructor-versioned
   #:proxy-destroy
   #:proxy-add-listener
   #:proxy-get-listener
   #:proxy-add-dispatcher
   #:proxy-set-user-data
   #:proxy-get-user-data
   #:proxy-get-version
   #:proxy-get-id
   #:proxy-set-tag
   #:proxy-get-tag
   #:proxy-get-class
   #:proxy-set-queue
   #:display-connect
   #:display-connect-to-fd
   #:display-disconnect
   #:display-get-fd
   #:display-dispatch
   #:display-dispatch-queue
   #:display-dispatch-queue-pending
   #:display-dispatch-pending
   #:display-get-error
   #:display-get-protocol-error
   #:display-flush
   #:display-roundtrip-queue
   #:display-roundtrip
   #:display-create-queue
   #:display-prepare-read-queue
   #:display-prepare-read
   #:display-cancel-read
   #:display-read-events
   #:log-set-handler-client
   #:define-listener
   #:buffer-interface
   #:callback-interface
   #:compositor-interface
   #:registry-interface
   #:seat-interface
   #:shell-interface
   #:shell-surface-interface
   #:shm-interface
   #:shm-pool-interface
   #:surface-interface
   #:seat-capabilities
   #:buffer-destroy
   #:compositor-create-surface
   #:display-get-registry
   #:pointer-set-cursor
   #:registry-bind
   #:seat-get-keyboard
   #:seat-get-pointer
   #:seat-get-touch
   #:shell-get-shell-surface
   #:shell-surface-pong
   #:shell-surface-set-title
   #:shell-surface-set-toplevel
   #:shm-create-pool
   #:shm-pool-create-buffer
   #:shm-pool-resize
   #:surface-attach
   #:surface-commit
   #:surface-damage
   #:surface-frame))

(defpackage #:org.shirakumo.framebuffers.wayland
  (:use #:cl)
  (:shadow #:atom)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int)
   (#:wl #:org.shirakumo.framebuffers.wayland.cffi))
  (:export
   #:window))
