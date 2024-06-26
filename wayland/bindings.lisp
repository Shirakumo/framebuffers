(in-package #:org.shirakumo.framebuffers.wayland.cffi)

(cffi:define-foreign-library wayland
  (:linux (:or (:default "libwayland-client") "libwayland-client.so.0"))
  (T (:or (:default "libwayland-client") (:default "wayland-client"))))

(cffi:define-foreign-library xkbcommon
  (:linux (:or (:default "libxkbcommon") "libxkbcommon.so.0"))
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

;; KLUDGE: work around for (setf (cffi:mem-aref x '(:union y)) z) being a no-op
(defmacro %set-argument (val var i)
  `(etypecase ,val
     (string
      (setf (cffi:mem-aref ,var :string ,i) ,val))
     (cffi:foreign-pointer
      (setf (cffi:mem-aref ,var :pointer ,i) ,val))
     (integer
      (setf (cffi:mem-aref ,var :int64 ,i) ,val))))

(defmacro define-vararg (name lambda-list call)
  (let ((args (car (last lambda-list)))
        (arglist (gensym "ARGLIST")))
    `(progn (defun ,name ,lambda-list
              (if ,args
                  (cffi:with-foreign-objects ((,arglist '(:union argument) (length ,args)))
                    (loop for i from 0
                          for arg in ,args
                          do (%set-argument arg ,arglist i))
                    (let ((,args ,arglist))
                      ,call))
                  (let ((,args (cffi:null-pointer)))
                    ,call)))
            (define-compiler-macro ,name ,lambda-list
              (if ,args
                  `(cffi:with-foreign-objects ((,',arglist '(:union argument) ,(length ,args)))
                     ,@(loop for i from 0
                             for arg in ,args
                             collect `(%set-argument ,arg ,',arglist ,i))
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
(defconstant XDG-ACTIVATION-V1-DESTROY 0)
(defconstant XDG-ACTIVATION-V1-GET-ACTIVATION-TOKEN 1)
(defconstant XDG-ACTIVATION-V1-ACTIVATE 2)
(defconstant XDG-ACTIVATION-TOKEN-V1-SET-SERIAL 0)
(defconstant XDG-ACTIVATION-TOKEN-V1-SET-APP-ID 1)
(defconstant XDG-ACTIVATION-TOKEN-V1-SET-SURFACE 2)
(defconstant XDG-ACTIVATION-TOKEN-V1-COMMIT 3)
(defconstant XDG-ACTIVATION-TOKEN-V1-DESTROY 4)
(defconstant ZWP-IDLE-INHIBIT-MANAGER-V1-DESTROY 0)
(defconstant ZWP-IDLE-INHIBIT-MANAGER-V1-CREATE-INHIBITOR 1)
(defconstant ZWP-IDLE-INHIBITOR-V1-DESTROY 0)
(defconstant WP-FRACTIONAL-SCALE-MANAGER-V1-DESTROY 0)
(defconstant WP-FRACTIONAL-SCALE-MANAGER-V1-GET-FRACTIONAL-SCALE 1)
(defconstant WP-FRACTIONAL-SCALE-V1-DESTROY 0)

(cffi:defcvar (buffer-interface "wl_buffer_interface") (:struct interface))
(cffi:defcvar (callback-interface "wl_callback_interface") (:struct interface))
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
(cffi:defcvar (output-interface "wl_output_interface") (:struct interface))

(defvar *interfaces* (make-hash-table :test 'eql))

(defun make-message-types (types)
  (if types
      (let ((ptrs (cffi:foreign-alloc :pointer :count (length types))))
        (loop for i from 0
              for type in types
              do (setf (cffi:mem-aref ptrs :pointer i)
                       (etypecase type
                         (null (cffi:null-pointer))
                         (cffi:foreign-pointer type)
                         (symbol (if (fboundp type)
                                     (funcall type)
                                     (or (cffi:get-var-pointer type)
                                         (error "No such variable ~s" type))))
                         (string (or (cffi:foreign-symbol-pointer type)
                                     (error "No such variable ~s" type))))))
        ptrs)
      (cffi:null-pointer)))

(defun make-messages (messages)
  (let ((ptrs (cffi:foreign-alloc '(:struct message) :count (length messages))))
    (loop for i from 0
          for (name signature . types) in messages
          for ptr = (cffi:mem-aptr ptrs '(:struct message) i)
          do (setf (message-name ptr) name)
             (setf (message-signature ptr) signature)
             (setf (message-types ptr) (make-message-types types)))
    ptrs))

(defun make-interface (sname name version methods events)
  (let ((iface (cffi:foreign-alloc '(:struct interface))))
    (setf (gethash sname *interfaces*) iface)
    (setf (interface-name iface) name)
    (setf (interface-version iface) version)
    (setf (interface-event-count iface) (length events))
    (setf (interface-events iface) (make-messages events))
    (setf (interface-method-count iface) (length methods))
    (setf (interface-methods iface) (make-messages methods))
    iface))

(defmacro define-interface (sname &body body)
  (destructuring-bind (&key name (version 1) methods events) body
      `(progn
         (defun ,sname ()
           (or (gethash ',sname *interfaces*)
               (setf (gethash ',sname *interfaces*)
                     (make-interface ',sname ,name ,version ',methods ',events))))

         (define-symbol-macro ,sname (,sname)))))

(define-interface compositor
  :name "wl_compositor"
  :methods (("create_surface" "n" NIL)
            ("create_region" "n" NIL)))

(define-interface xdg-activation-v1-interface
  :name "xdg_activation_v1"
  :methods (("destroy" "")
            ("get_activation_token" "n" xdg-activation-token-v1-interface)
            ("activate" "so" NIL surface-interface)))

(define-interface xdg-activation-token-v1-interface
  :name "xdg_activation_token_v1"
  :methods (("set_serial" "uo" NIL seat-interface)
            ("set_app_id" "s" NIL)
            ("set_surface" "o" seat-interface)
            ("commit" "")
            ("destroy" ""))
  :events (("done" "s" NIL)))

(define-interface wp-fractional-scale-manager-v1-interface
  :name "wp_fractional_scale_manager_v1"
  :methods (("destroy" "")
            ("get_fractional_scale" "no" wp-fractional-scale-v1-interface surface-interface)))

(define-interface wp-fractional-scale-v1-interface
  :name "wp_fractional_scale_v1"
  :methods (("destroy" ""))
  :events (("preferred_scale" "u" NIL)))

(define-interface zwp-idle-inhibit-manager-v1-interface
  :name "zwp_idle_inhibit_manager_v1"
  :methods (("destroy" "")
            ("create_inhibitor" "no" zwp-idle-inhibitor-v1-interface surface-interface)))

(define-interface zwp-idle-inhibitor-v1-interface
  :name "zwp_idle_inhibitor_v1"
  :methods (("destroy" "")))

(define-interface zxdg-decoration-manager-v1-interface
  :name "zxdg_decoration_manager_v1"
  :methods (("destroy" "")
            ("get_toplevel_decoration" "no" zxdg-toplevel-decoration-v1-interface xdg-toplevel-interface)))

(define-interface zxdg-toplevel-decoration-v1-interface
  :name "zxdg_toplevel_decoration_v1"
  :methods (("destroy" "")
            ("set_mode" "u" NIL)
            ("unset_mode" "")))

(define-interface xdg-wm-base-interface
  :name "xdg_wm_base"
  :version 6
  :methods (("destroy" "")
            ("create_positioner" "n" xdg-positioner-interface)
            ("get_xdg_surface" "no" xdg-surface-interface surface-interface)
            ("pong" "u" NIL))
  :events (("ping" "u" NIL)))

(define-interface xdg-popup-interface
  :name "xdg_popup_interface"
  :version 6
  :methods (("destroy" ""))
  :events (("popup_done" "")))

(define-interface xdg-positioner-interface
  :name "xdg_positioner"
  :version 6
  :methods (("destroy" "")
            ("set_size" "ii" NIL NIL)
            ("set_anchor_rect" "iiii" NIL NIL NIL NIL)
            ("set_anchor" "u" NIL)
            ("set_gravity" "u" NIL)
            ("set_constraint_adjustment" "u" NIL)
            ("set_offset" "ii" NIL NIL)
            ("set_reactive" "3" NIL NIL NIL)
            ("set_parent_size" "3ii" NIL NIL NIL)
            ("set_parent_configure" "3u" NIL NIL NIL)))

(define-interface xdg-surface-interface
  :name "xdg_surface"
  :version 6
  :methods (("destroy" "")
            ("get_toplevel" "n" xdg-toplevel-interface)
            ("get_popup" "n?oo" xdg-popup-interface xdg-surface-interface xdg-positioner-interface)
            ("set_window_geometry" "iiii" NIL NIL NIL NIL)
            ("ack_configure" "u" NIL))
  :events (("configure" "u" NIL)))

(define-interface xdg-toplevel-interface
  :name "xdg_toplevel"
  :version 6
  :methods (("destroy" "")
            ("set_parent" "?o" xdg-toplevel-interface)
            ("set_title" "s" NIL)
            ("set_app_id" "s" NIL)
            ("show_window_menu" "ouii" seat-interface NIL NIL NIL)
            ("move" "ou" seat-interface NIL)
            ("resize" "ouu" seat-interface NIL NIL)
            ("set_max_size" "ii" NIL NIL)
            ("set_min_size" "ii" NIL NIL)
            ("set_maximized" "")
            ("unset_maximized" "")
            ("set_fullscreen" "?o" output-interface)
            ("unset_fullscreen" "")
            ("set_minimized" ""))
  :events (("configure" "iia" NIL NIL NIL)
           ("close" "")
           ("configure_bounds" "4ii" NIL NIL NIL)
           ("wm_capabilities" "5a" NIL NIL NIL xdg-positioner-interface)))

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
                            ,(cond ((null interface)
                                    '(cffi:null-pointer))
                                   ((fboundp interface)
                                    `(,interface))
                                   ((symbolp interface)
                                    `(cffi:get-var-pointer ',interface)))
                            (proxy-get-version ,object)
                            0
                            ,@(loop for arg in args collect (or arg '(cffi:null-pointer)))))))

(defmacro define-destroy-fun (name)
  `(defun ,name (object)
     (proxy-marshal-flags object ,name (cffi:null-pointer) (proxy-get-version object) MARSHAL-FLAG-DESTROY)))

(define-destroy-fun buffer-destroy)

(define-marshal-fun compositor-create-surface surface-interface (NIL))

(define-marshal-fun display-get-registry registry-interface (NIL))

(define-marshal-fun pointer-set-cursor NIL (serial surface hotspot-x hotspot-y))

(defun registry-bind (registry name interface version)
  (proxy-marshal-flags registry REGISTRY-BIND interface version 0
                       name (cffi:mem-ref (cffi:foreign-slot-pointer interface '(:struct interface) 'name) :pointer) version (cffi:null-pointer)))

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

(define-marshal-fun xdg-surface-get-toplevel xdg-toplevel-interface (NIL))

(define-marshal-fun xdg-surface-ack-configure NIL (serial))

(define-marshal-fun xdg-toplevel-set-maximized NIL ())

(define-marshal-fun xdg-toplevel-set-max-size NIL (width height))

(define-marshal-fun xdg-toplevel-unset-maximized NIL ())

(define-marshal-fun xdg-toplevel-set-fullscreen NIL (output))

(define-marshal-fun xdg-toplevel-unset-fullscreen NIL ())

(define-marshal-fun xdg-toplevel-set-minimized NIL ())

(define-marshal-fun xdg-toplevel-set-title NIL (title))

(define-destroy-fun zxdg-toplevel-decoration-v1-destroy)

(define-destroy-fun xdg-activation-v1-destroy)

(define-marshal-fun xdg-activation-v1-get-activation-token xdg-activation-token-v1-interface (NIL))

(define-marshal-fun xdg-activation-v1-activate NIL (token surface))

(define-marshal-fun xdg-activation-token-v1-set-serial NIL (serial seat))

(define-marshal-fun xdg-activation-token-v1-set-surface NIL (surface))

(define-marshal-fun xdg-activation-token-v1-commit NIL ())

(define-destroy-fun xdg-activation-token-v1-destroy)

(define-destroy-fun zwp-idle-inhibit-manager-v1-destroy)

(define-marshal-fun zwp-idle-inhibit-manager-v1-create-inhibitor zwp-idle-inhibitor-v1-interface (NIL surface))

(define-destroy-fun zwp-idle-inhibitor-v1-destroy)

(define-destroy-fun wp-fractional-scale-manager-v1-destroy)

(define-marshal-fun wp-fractional-scale-manager-v1-get-fractional-scale wp-fractional-scale-v1-interface (surface))

(define-destroy-fun wp-fractional-scale-v1-destroy)

;;; xkbcommon
(cffi:defbitfield xkb-state-component
  (:mods-depressed 1)
  :mods-latched
  :mods-locked
  :mods-effective
  :layout-depressed
  :layout-latched
  :layout-locked
  :layout-effective
  :state-leds)

(cffi:defcenum xkb-compose-feed-result
  :ignored
  :accepted)

(cffi:defcenum xkb-compose-status
  :nothing
  :composing
  :composed
  :cancelled)

(cffi:defcfun (xkb-context-new "xkb_context_new") :pointer
  (flags :int))

(cffi:defcfun (xkb-context-unref "xkb_context_unref") :void
  (context :pointer))

(cffi:defcfun (xkb-keymap-new-from-string "xkb_keymap_new_from_string") :pointer
  (context :pointer)
  (string :pointer)
  (format :int)
  (flags :int))

(cffi:defcfun (xkb-keymap-unref "xkb_keymap_unref") :void
  (keymap :pointer))

(cffi:defcfun (xkb-keymap-mod-get-index "xkb_keymap_mod_get_index") :uint32
  (keymap :pointer)
  (name :string))

(cffi:defcfun (xkb-keymap-key-repeats "xkb_keymap_key_repeats") :int
  (keymap :pointer)
  (key :uint32))

(cffi:defcfun (xkb-keymap-key-get-syms-by-level "xkb_keymap_key_get_syms_by_level") :int
  (keymap :pointer)
  (key :uint32)
  (layout :int)
  (level :int)
  (syms :pointer))

(cffi:defcfun (xkb-state-new "xkb_state_new") :pointer
  (keymap :pointer))

(cffi:defcfun (xkb-state-unref "xkb_state_unref") :void
  (state :pointer))

(cffi:defcfun (xkb-state-key-get-syms "xkb_state_key_get_syms") :int
  (state :pointer)
  (key :uint32)
  (syms :pointer))

(cffi:defcfun (xkb-state-update-mask "xkb_state_update_mask") :int
  (state :pointer)
  (depressed-mods :uint32)
  (latched-mods :uint32)
  (locked-mods :uint32)
  (depressed-layout :uint32)
  (latched-layout :uint32)
  (locked-layout :uint32))

(cffi:defcfun (xkb-state-key-get-layout "xkb_state_key_get_layout") :uint32
  (state :pointer)
  (key :uint32))

(cffi:defcfun (xkb-state-mod-index-is-active "xkb_state_mod_index_is_active") :int
  (state :pointer)
  (mod :uint32)
  (type xkb-state-component))

(cffi:defcfun (xkb-compose-table-new-from-locale "xkb_compose_table_new_from_locale") :pointer
  (context :pointer)
  (locale :string)
  (flags :int))

(cffi:defcfun (xkb-compose-table-unref "xkb_compose_table_unref") :void
  (table :pointer))

(cffi:defcfun (xkb-compose-state-new "xkb_compose_state_new") :pointer
  (table :pointer)
  (flags :int))

(cffi:defcfun (xkb-compose-state-unref "xkb_compose_state_unref") :void
  (state :pointer))

(cffi:defcfun (xkb-compose-state-feed "xkb_compose_state_feed") xkb-compose-feed-result
  (state :pointer)
  (keysym :uint32))

(cffi:defcfun (xkb-compose-state-get-status "xkb_compose_state_get_status") xkb-compose-status
  (state :pointer))

(cffi:defcfun (xkb-compose-state-get-one-sym "xkb_compose_state_get_one_sym") :uint32
  (state :pointer))

(cffi:defcfun (xkb-compose-state-get-utf8 "xkb_compose_state_get_utf8") :int
  (state :pointer)
  (buffer :pointer)
  (size :size))
