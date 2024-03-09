(in-package #:org.shirakumo.framebuffers.wayland.cffi)

(cffi:define-foreign-library wayland
  (T (:or (:default "libwayland-client") (:default "wayland-client"))))

;;; core defs
(cffi:defcstruct (message :conc-name message-)
  (name :string)
  (signature :string)
  (types :pointer))

(cffi:defcstruct (interface :conc-name interface-)
  (name :string)
  (version :int)
  (method-count :int)
  (methods :pointer)
  (event-count :int)
  (events :pointer))

(cffi:defcunion argument
  (i :int32)
  (u :uint32)
  (f :int32)
  (s :string)
  (o :pointer)
  (n :uint32)
  (a :pointer)
  (h :int32))

(cffi:defcfun (event-queue-destroy "wl_event_queue_destroy") :void
  (queue :pointer))

(cffi:defcfun (proxy-marshal-array-flags "wl_proxy_marshal_array_flags") :pointer
  (proxy :pointer)
  (opcode :uint32)
  (interface :pointer)
  (version :uint32)
  (flags :uint32)
  (args :pointer))

(cffi:defcfun (proxy-marshal-array "wl_proxy_marshal_array") :void
  (p :pointer)
  (opcode :uint32)
  (args :pointer))

(cffi:defcfun (proxy-create "wl_proxy_create") :pointer
  (factory :pointer)
  (interface :pointer))

(cffi:defcfun (proxy-create-wrapper "wl_proxy_create_wrapper") :pointer
  (proxy :pointer))

(cffi:defcfun (proxy-wrapper-destroy "wl_proxy_wrapper_destroy") :void
  (proxy_wrapper :pointer))

(cffi:defcfun (proxy-marshal-array-constructor "wl_proxy_marshal_array_constructor") :pointer
  (proxy :pointer)
  (opcode :uint32)
  (args :pointer)
  (interface :pointer))

(cffi:defcfun (proxy-marshal-array-constructor-versioned "wl_proxy_marshal_array_constructor_versioned") :pointer
  (proxy :pointer)
  (opcode :uint32)
  (args :pointer)
  (interface :pointer)
  (version :uint32))

(cffi:defcfun (proxy-destroy "wl_proxy_destroy") :void
  (proxy :pointer))

(cffi:defcfun (proxy-add-listener "wl_proxy_add_listener") :int
  (proxy :pointer)
  (implementation :pointer)
  (data :pointer))

(cffi:defcfun (proxy-get-listener "wl_proxy_get_listener") :pointer
  (proxy :pointer))

(cffi:defcfun (proxy-add-dispatcher "wl_proxy_add_dispatcher") :int
  (proxy :pointer)
  (dispatcher_func :pointer)
  (dispatcher_data :pointer)
  (data :pointer))

(cffi:defcfun (proxy-set-user-data "wl_proxy_set_user_data") :void
  (proxy :pointer)
  (user_data :pointer))

(cffi:defcfun (proxy-get-user-data "wl_proxy_get_user_data") :pointer
  (proxy :pointer))

(cffi:defcfun (proxy-get-version "wl_proxy_get_version") :uint32
  (proxy :pointer))

(cffi:defcfun (proxy-get-id "wl_proxy_get_id") :uint32
  (proxy :pointer))

(cffi:defcfun (proxy-set-tag "wl_proxy_set_tag") :void
  (proxy :pointer)
  (tag :pointer))

(cffi:defcfun (proxy-get-tag "wl_proxy_get_tag") :pointer
  (proxy :pointer))

(cffi:defcfun (proxy-get-class "wl_proxy_get_class") :string
  (proxy :pointer))

(cffi:defcfun (proxy-set-queue "wl_proxy_set_queue") :void
  (proxy :pointer)
  (queue :pointer))

(cffi:defcfun (display-connect "wl_display_connect") :pointer
  (name :string))

(cffi:defcfun (display-connect-to-fd "wl_display_connect_to_fd") :pointer
  (fd :int))

(cffi:defcfun (display-disconnect "wl_display_disconnect") :void
  (display :pointer))

(cffi:defcfun (display-get-fd "wl_display_get_fd") :int
  (display :pointer))

(cffi:defcfun (display-dispatch "wl_display_dispatch") :int
  (display :pointer))

(cffi:defcfun (display-dispatch-queue "wl_display_dispatch_queue") :int
  (display :pointer)
  (queue :pointer))

(cffi:defcfun (display-dispatch-queue-pending "wl_display_dispatch_queue_pending") :int
  (display :pointer)
  (queue :pointer))

(cffi:defcfun (display-dispatch-pending "wl_display_dispatch_pending") :int
  (display :pointer))

(cffi:defcfun (display-get-error "wl_display_get_error") :int
  (display :pointer))

(cffi:defcfun (display-get-protocol-error "wl_display_get_protocol_error") :uint32
  (display :pointer)
  (interface :pointer)
  (id :pointer))

(cffi:defcfun (display-flush "wl_display_flush") :int
  (display :pointer))

(cffi:defcfun (display-roundtrip-queue "wl_display_roundtrip_queue") :int
  (display :pointer)
  (queue :pointer))

(cffi:defcfun (display-roundtrip "wl_display_roundtrip") :int
  (display :pointer))

(cffi:defcfun (display-create-queue "wl_display_create_queue") :pointer
  (display :pointer))

(cffi:defcfun (display-prepare-read-queue "wl_display_prepare_read_queue") :int
  (display :pointer)
  (queue :pointer))

(cffi:defcfun (display-prepare-read "wl_display_prepare_read") :int
  (display :pointer))

(cffi:defcfun (display-cancel-read "wl_display_cancel_read") :void
  (display :pointer))

(cffi:defcfun (display-read-events "wl_display_read_events") :int
  (display :pointer))

(cffi:defcfun (log-set-handler-client "wl_log_set_handler_client") :void
  (handler :pointer))

;;; protocol defs
(defun buffer-destroy ())
(defun callback-add-listener ())
(defun callback-destroy ())
(defun compositor-create-surface ())
(defun compositor-destroy ())
(defun cursor-image-get-buffer ())
(defun display-get-registry ())
(defun keyboard-add-listener ())
(defun keyboard-destroy ())
(defun pointer-destroy ())
(defun pointer-set-cursor ())
(defun registry-add-listener ())
(defun registry-bind ())
(defun registry-destroy ())
(defun seat-add-listener ())
(defun seat-destroy ())
(defun shell-destroy ())
(defun shell-get-shell-surface ())
(defun shell-surface-add-listener ())
(defun shell-surface-destroy ())
(defun shell-surface-pong ())
(defun shell-surface-set-title ())
(defun shell-surface-set-toplevel ())
(defun shm-add-listener ())
(defun shm-create-pool ())
(defun shm-destroy ())
(defun shm-pool-create-buffer ())
(defun shm-pool-resize ())
(defun surface-attach ())
(defun surface-commit ())
(defun surface-damage ())
(defun surface-frame ())
