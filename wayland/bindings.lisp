(in-package #:org.shirakumo.framebuffers.wayland.cffi)

(cffi:define-foreign-library wayland
  (T (:or (:default "libwayland-client") (:default "wayland-client"))))

(cffi:define-foreign-library xkbcommon
  (T (:or (:default "libxkbcommon") (:default "xkbcommon"))))

;;; core defs
(defconstant MARSHAL-FLAG-DESTROY 1)

(cffi:defcstruct (array :conc-name array-)
  (size :size)
  (alloc :size)
  (data :pointer))

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

(defmacro define-vararg (name lambda-list call)
  (let ((args (car (last lambda-list)))
        (arglist (gensym "ARGLIST")))
    `(progn (defun ,name ,lambda-list
              (if ,args
                  (cffi:with-foreign-objects ((,arglist '(:union argument) (length ,args)))
                    (loop for i from 0
                          for arg in ,args
                          do (setf (cffi:mem-aref ,arglist '(:union argument) i) arg))
                    (let ((,args ,arglist))
                      ,call))
                  (let ((,args (cffi:null-pointer)))
                    ,call)))
            (define-compiler-macro ,name ,lambda-list
              (if ,args
                  `(cffi:with-foreign-objects ((,',arglist '(:union argument) ,(length ,args)))
                     ,@(loop for i from 0
                             for arg in ,args
                             collect `(setf (cffi:mem-aref ,',arglist '(:union argument) ,i) ,arg))
                     (let ((,',args ,',arglist)
                           ,@(list ,@(loop for arg in lambda-list
                                           until (eql arg '&rest)
                                           collect `(list ',arg ,arg))))
                       ,',call))
                  `(let ((,',args (cffi:null-pointer))
                         ,@(list ,@(loop for arg in lambda-list
                                         until (eql arg '&rest)
                                         collect `(list ',arg ,arg))))
                     ,',call))))))

(define-vararg proxy-marshal-flags (proxy opcode interface version flags &rest args)
  (proxy-marshal-array-flags proxy opcode interface version flags args))

(cffi:defcfun (proxy-marshal-array "wl_proxy_marshal_array") :void
  (p :pointer)
  (opcode :uint32)
  (args :pointer))

(define-vararg proxy-marshal (proxy opcode &rest args)
  (proxy-marshal-array proxy opcode args))

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

(define-vararg proxy-marshal-constructor (proxy opcode interface &rest args)
  (proxy-marshal-array-constructor proxy opcode args interface))

(cffi:defcfun (proxy-marshal-array-constructor-versioned "wl_proxy_marshal_array_constructor_versioned") :pointer
  (proxy :pointer)
  (opcode :uint32)
  (args :pointer)
  (interface :pointer)
  (version :uint32))

(define-vararg proxy-marshal-constructor-versioned (proxy opcode interface version &rest args)
  (proxy-marshal-array-constructor-versioned proxy opcode args interface version))

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
(defconstant DISPLAY-SYNC 0)
(defconstant DISPLAY-GET-REGISTRY 1)
(defconstant REGISTRY-BIND 0)
(defconstant COMPOSITOR-CREATE-SURFACE 0)
(defconstant COMPOSITOR-CREATE-REGION 1)
(defconstant SHM-POOL-CREATE-BUFFER 0)
(defconstant SHM-POOL-DESTROY 1)
(defconstant SHM-POOL-RESIZE 2)
(defconstant SHM-CREATE-POOL 0)
(defconstant BUFFER-DESTROY 0)
(defconstant DATA-OFFER-ACCEPT 0)
(defconstant DATA-OFFER-RECEIVE 1)
(defconstant DATA-OFFER-DESTROY 2)
(defconstant DATA-OFFER-FINISH 3)
(defconstant DATA-OFFER-SET-ACTIONS 4)
(defconstant DATA-SOURCE-OFFER 0)
(defconstant DATA-SOURCE-DESTROY 1)
(defconstant DATA-SOURCE-SET-ACTIONS 2)
(defconstant DATA-DEVICE-START-DRAG 0)
(defconstant DATA-DEVICE-SET-SELECTION 1)
(defconstant DATA-DEVICE-RELEASE 2)
(defconstant DATA-DEVICE-MANAGER-CREATE-DATA-SOURCE 0)
(defconstant DATA-DEVICE-MANAGER-GET-DATA-DEVICE 1)
(defconstant SHELL-GET-SHELL-SURFACE 0)
(defconstant SHELL-SURFACE-PONG 0)
(defconstant SHELL-SURFACE-MOVE 1)
(defconstant SHELL-SURFACE-RESIZE 2)
(defconstant SHELL-SURFACE-SET-TOPLEVEL 3)
(defconstant SHELL-SURFACE-SET-TRANSIENT 4)
(defconstant SHELL-SURFACE-SET-FULLSCREEN 5)
(defconstant SHELL-SURFACE-SET-POPUP 6)
(defconstant SHELL-SURFACE-SET-MAXIMIZED 7)
(defconstant SHELL-SURFACE-SET-TITLE 8)
(defconstant SHELL-SURFACE-SET-CLASS 9)
(defconstant SURFACE-DESTROY 0)
(defconstant SURFACE-ATTACH 1)
(defconstant SURFACE-DAMAGE 2)
(defconstant SURFACE-FRAME 3)
(defconstant SURFACE-SET-OPAQUE-REGION 4)
(defconstant SURFACE-SET-INPUT-REGION 5)
(defconstant SURFACE-COMMIT 6)
(defconstant SURFACE-SET-BUFFER-TRANSFORM 7)
(defconstant SURFACE-SET-BUFFER-SCALE 8)
(defconstant SURFACE-DAMAGE-BUFFER 9)
(defconstant SURFACE-OFFSET 10)
(defconstant SEAT-GET-POINTER 0)
(defconstant SEAT-GET-KEYBOARD 1)
(defconstant SEAT-GET-TOUCH 2)
(defconstant SEAT-RELEASE 3)
(defconstant POINTER-SET-CURSOR 0)
(defconstant POINTER-RELEASE 1)
(defconstant KEYBOARD-RELEASE 0)
(defconstant TOUCH-RELEASE 0)
(defconstant OUTPUT-RELEASE 0)
(defconstant REGION-DESTROY 0)
(defconstant REGION-ADD 1)
(defconstant REGION-SUBTRACT 2)
(defconstant SUBCOMPOSITOR-DESTROY 0)
(defconstant SUBCOMPOSITOR-GET-SUBSURFACE 1)
(defconstant SUBSURFACE-DESTROY 0)
(defconstant SUBSURFACE-SET-POSITION 1)
(defconstant SUBSURFACE-PLACE-ABOVE 2)
(defconstant SUBSURFACE-PLACE-BELOW 3)
(defconstant SUBSURFACE-SET-SYNC 4)
(defconstant SUBSURFACE-SET-DESYNC 5)
(defconstant XDG-WM-BASE-DESTROY 0)
(defconstant XDG-WM-BASE-CREATE-POSITIONER 1)
(defconstant XDG-WM-BASE-GET-XDG-SURFACE 2)
(defconstant XDG-WM-BASE-PONG 3)
(defconstant XDG-POSITIONER-DESTROY 0)
(defconstant XDG-POSITIONER-SET-SIZE 1)
(defconstant XDG-POSITIONER-SET-ANCHOR-RECT 2)
(defconstant XDG-POSITIONER-SET-ANCHOR 3)
(defconstant XDG-POSITIONER-SET-GRAVITY 4)
(defconstant XDG-POSITIONER-SET-CONSTRAINT-ADJUSTMENT 5)
(defconstant XDG-POSITIONER-SET-OFFSET 6)
(defconstant XDG-POSITIONER-SET-REACTIVE 7)
(defconstant XDG-POSITIONER-SET-PARENT-SIZE 8)
(defconstant XDG-POSITIONER-SET-PARENT-CONFIGURE 9)
(defconstant XDG-SURFACE-DESTROY 0)
(defconstant XDG-SURFACE-GET-TOPLEVEL 1)
(defconstant XDG-SURFACE-GET-POPUP 2)
(defconstant XDG-SURFACE-SET-WINDOW-GEOMETRY 3)
(defconstant XDG-SURFACE-ACK-CONFIGURE 4)
(defconstant XDG-TOPLEVEL-DESTROY 0)
(defconstant XDG-TOPLEVEL-SET-PARENT 1)
(defconstant XDG-TOPLEVEL-SET-TITLE 2)
(defconstant XDG-TOPLEVEL-SET-APP-ID 3)
(defconstant XDG-TOPLEVEL-SHOW-WINDOW-MENU 4)
(defconstant XDG-TOPLEVEL-MOVE 5)
(defconstant XDG-TOPLEVEL-RESIZE 6)
(defconstant XDG-TOPLEVEL-SET-MAX-SIZE 7)
(defconstant XDG-TOPLEVEL-SET-MIN-SIZE 8)
(defconstant XDG-TOPLEVEL-SET-MAXIMIZED 9)
(defconstant XDG-TOPLEVEL-UNSET-MAXIMIZED 10)
(defconstant XDG-TOPLEVEL-SET-FULLSCREEN 11)
(defconstant XDG-TOPLEVEL-UNSET-FULLSCREEN 12)
(defconstant XDG-TOPLEVEL-SET-MINIMIZED 13)
(defconstant XDG-POPUP-DESTROY 0)
(defconstant XDG-POPUP-GRAB 1)
(defconstant XDG-POPUP-REPOSITION 2)
(defconstant ZXDG-TOPLEVEL-DECORATION-V1-DESTROY 0)

(cffi:defcvar (buffer-interface "wl_buffer_interface") (:struct interface))
(cffi:defcvar (callback-interface "wl_callback_interface") (:struct interface))
(cffi:defcvar (compositor-interface "wl_compositor_interface") (:struct interface))
(cffi:defcvar (keyboard-interface "wl_keyboard_interface") (:struct interface))
(cffi:defcvar (pointer-interface "wl_pointer_interface") (:struct interface))
(cffi:defcvar (registry-interface "wl_registry_interface") (:struct interface))
(cffi:defcvar (seat-interface "wl_seat_interface") (:struct interface))
(cffi:defcvar (shell-interface "wl_shell_interface") (:struct interface))
(cffi:defcvar (shell-surface-interface "wl_shell_surface_interface") (:struct interface))
(cffi:defcvar (shm-interface "wl_shm_interface") (:struct interface))
(cffi:defcvar (shm-pool-interface "wl_shm_pool_interface") (:struct interface))
(cffi:defcvar (surface-interface "wl_surface_interface") (:struct interface))
(cffi:defcvar (touch-interface "wl_touch_interface") (:struct interface))
(cffi:defcvar (xdg-toplevel-interface "xdg_toplevel_interface") (:struct interface))
(cffi:defcvar (xdg-wm-base-interface "xdg_wm_base_interface") (:struct interface))
(cffi:defcvar (zxdg-decoration-manager-v1-interface "zxdg_decoration_manager_v1_interface") (:struct interface))

(cffi:defcenum (xdg-toplevel-state :uint32)
  (:maximized 1)
  (:fullscreen 2)
  (:resizing 3)
  (:activated 4)
  (:tiled-left 5)
  (:tiled-right 6)
  (:tiled-top 7)
  (:tiled-bottom 8)
  (:state-suspended 9))

(cffi:defbitfield seat-capabilities
  :pointer :keyboard :touch)

(defmacro define-listener (name &body callbacks)
  `(progn (cffi:defcstruct (,name :conc-name ,(intern (format NIL "~a-" (symbol-name name))))
            ,@(loop for (cb) in callbacks collect `(,cb :pointer)))
          
          ,@(loop for (cb ret args . body) in callbacks
                  when ret
                  collect `(cffi:defcallback ,(intern (format NIL "~a-~a" (symbol-name name) (symbol-name cb))) ,ret ,args 
                             ,@body))

          (defun ,(intern (format NIL "~a-~a" (symbol-name :make) name)) (&optional (struct (cffi:foreign-alloc '(:struct ,name))))
            ,@(loop for (cb ret args . body) in callbacks
                    for slot = (intern (format NIL "~a-~a" (symbol-name name) (symbol-name cb)))
                    collect `(setf (,slot struct) ,(if ret `(cffi:callback ,slot) '(cffi:null-pointer))))
            struct)))

(defmacro define-marshal-fun (name interface args)
  (let ((object (gensym "OBJECT")))
    `(defun ,name (,object ,@(loop for arg in args when (and arg (symbolp arg)) collect arg))
       (proxy-marshal-flags ,object
                            ,name
                            ,(if interface `(cffi:get-var-pointer ',interface) '(cffi:null-pointer))
                            (proxy-get-version ,object)
                            0
                            ,@(loop for arg in args collect (or arg '(cffi:null-pointer)))))))

(defun buffer-destroy (buffer)
  (proxy-marshal-flags buffer BUFFER-DESTROY (cffi:null-pointer) (proxy-get-version buffer) MARSHAL-FLAG-DESTROY))

(define-marshal-fun compositor-create-surface surface-interface (NIL))

(define-marshal-fun display-get-registry registry-interface (NIL))

(define-marshal-fun pointer-set-cursor NIL (serial surface hotspot-x hotspot-y))

(defun registry-bind (registry name interface version)
  (proxy-marshal-flags registry REGISTRY-BIND interface version 0 name (interface-name interface) version (cffi:null-pointer)))

(define-marshal-fun seat-get-keyboard keyboard-interface (NIL))

(define-marshal-fun seat-get-pointer pointer-interface (NIL))

(define-marshal-fun seat-get-touch touch-interface (NIL))

(define-marshal-fun shell-get-shell-surface shell-surface-interface (NIL surface))

(define-marshal-fun shell-surface-pong NIL (serial))

(define-marshal-fun shell-surface-set-title NIL (title))

(define-marshal-fun shell-surface-set-toplevel NIL ())

(define-marshal-fun shm-create-pool shm-pool-interface (NIL fd size))

(define-marshal-fun shm-pool-create-buffer buffer-interface (NIL offset width height stride format))

(define-marshal-fun shm-pool-resize NIL (size))

(define-marshal-fun surface-attach NIL (buffer x y))

(define-marshal-fun surface-commit NIL ())

(define-marshal-fun surface-damage NIL (x y width height))

(define-marshal-fun surface-frame callback-interface (NIL))

(define-marshal-fun xdg-wm-base-get-xdg-surface xdg-surface-interface ((cffi:null-pointer) surface))

(define-marshal-fun xdg-wm-base-pong NIL (serial))

(define-marshal-fun xdg-surface-get-toplevel xdg-toplevel-interface ())

(define-marshal-fun xdg-surface-ack-configure NIL (serial))

(define-marshal-fun xdg-toplevel-set-maximized NIL ())

(define-marshal-fun xdg-toplevel-set-max-size NIL (width height))

(define-marshal-fun xdg-toplevel-unset-maximized NIL ())

(define-marshal-fun xdg-toplevel-set-fullscreen NIL (output))

(define-marshal-fun xdg-toplevel-unset-fullscreen NIL ())

(define-marshal-fun xdg-toplevel-set-minimized NIL ())

(define-marshal-fun xdg-toplevel-set-title NIL (title))

(defun zxdg-toplevel-decoration-v1-destroy (object)
  (proxy-marshal-flags object ZXDG-TOPLEVEL-DECORATION-V1-DESTROY (cffi:null-pointer) (proxy-get-version object) MARSHAL-FLAG-DESTROY))

;;; xkbcommon
(cffi:defcfun (xkb-context-new "xkb_context_new"))

(cffi:defcfun (xkb-context-unref "xkb_context_unref"))

(cffi:defcfun (xkb-keymap-new-from-string "xkb_keymap_new_from_string"))

(cffi:defcfun (xkb-keymap-unref "xkb_keymap_unref"))

(cffi:defcfun (xkb-keymap-mod-get-index "xkb_keymap_mod_get_index"))

(cffi:defcfun (xkb-keymap-key-repeats "xkb_keymap_key_repeats"))

(cffi:defcfun (xkb-keymap-key-get-syms-by-level "xkb_keymap_key_get_syms_by_level"))

(cffi:defcfun (xkb-state-new "xkb_state_new"))

(cffi:defcfun (xkb-state-unref "xkb_state_unref"))

(cffi:defcfun (xkb-state-key-get-syms "xkb_state_key_get_syms"))

(cffi:defcfun (xkb-state-update-mask "xkb_state_update_mask"))

(cffi:defcfun (xkb-state-key-get-layout "xkb_state_key_get_layout"))

(cffi:defcfun (xkb-state-mod-index-is-active "xkb_state_mod_index_is_active"))

(cffi:defcfun (xkb-compose-table-new-from-locale "xkb_compose_table_new_from_locale"))

(cffi:defcfun (xkb-compose-table-unref "xkb_compose_table_unref"))

(cffi:defcfun (xkb-compose-state-new "xkb_compose_state_new"))

(cffi:defcfun (xkb-compose-state-unref "xkb_compose_state_unref"))

(cffi:defcfun (xkb-compose-state-feed "xkb_compose_state_feed"))

(cffi:defcfun (xkb-compose-state-get-status "xkb_compose_state_get_status"))

(cffi:defcfun (xkb-compose-state-get-one-sym "xkb_compose_state_get_one_sym"))
