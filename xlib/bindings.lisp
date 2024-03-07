(in-package #:org.shirakumo.framebuffers.xlib.cffi)

(cffi:define-foreign-library x11
  (:defaults "X11"))

(cffi:defctype xid :ulong)
(cffi:defctype atom :ulong)

(cffi:defcenum event-type
  (:key-press 2)
  (:key-release 3)
  (:button-press 4)
  (:button-release 5)
  (:motion-notify 6)
  (:enter-notify 7)
  (:leave-notify 8)
  (:focus-in 9)
  (:focus-out 10)
  (:keymap-notify 11)
  (:expose 12)
  (:graphics-expose 13)
  (:no-expose 14)
  (:visibility-notify 15)
  (:create-notify 16)
  (:destroy-notify 17)
  (:unmap-notify 18)
  (:map-notify 19)
  (:map-request 20)
  (:reparent-notify 21)
  (:configure-notify 22)
  (:configure-request 23)
  (:gravity-notify 24)
  (:resize-request 25)
  (:circulate-notify 26)
  (:circulate-request 27)
  (:property-notify 28)
  (:selection-clear 29)
  (:selection-request 30)
  (:selection-notify 31)
  (:colormap-notify 32)
  (:client-message 33)
  (:mapping-notify 34)
  (:generic-event 35))

(cffi:defcstruct (image :conc-name image-)
  (width :int)
  (height :int)
  (xoffset :int)
  (format :int)
  (data :pointer)
  (byte-order :int)
  (bitmap-unit :int)
  (bitmap-bit-order :int)
  (bitmap-pad :int)
  (depth :int)
  (bytes-per-line :int)
  (bits-per-pixel :int)
  (red-mask :ulong)
  (green-mask :ulong)
  (blue-mask :ulong)
  (obdata :pointer)
  (create-image :pointer)
  (destroy-image :pointer)
  (get-pixel :pointer)
  (put-pixel :pointer)
  (sub-image :pointer)
  (add-pixel :pointer))

(cffi:defcstruct (set-window-attributes :conc-name set-window-attributes-)
  (background-pixmap xid)
  (background-pixel :ulong)
  (border-pixmap xid)
  (border-pixel :ulong)
  (bit-gravity :int)
  (win-gravity :int)
  (backing-store :int)
  (backing-planes :ulong)
  (backing-pixel :ulong)
  (save-under :bool)
  (event-mask :long)
  (do-not-propagate-mask :long)
  (override-redirect :bool)
  (colormap xid)
  (cursor xid))

(cffi:defcstruct (pixmap-format :conc-name pixmap-format-)
  (depth :int)
  (bits-per-pixel :int)
  (scanline-pad :int))

(cffi:defcstruct (size-hint :conc-name size-hint-)
  (flags :long)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (width-inc :int)
  (height-inc :int)
  (min-aspect-x :int)
  (min-aspect-y :int)
  (max-aspect-x :int)
  (max-aspect-y :int)
  (base-width :int)
  (base-height :int)
  (win-gravity :int))

(cffi:defcstruct event
  (_ :char :count 192))

(cffi:defcstruct (base-event :conc-name base-event-)
  (type event-type)
  (serial :ulong)
  (send-event :bool)
  (display :pointer)
  (window xid))

(cffi:defcstruct (positioned-event :conc-name positioned-event-)
  (_ (:struct base-event))
  (root xid)
  (subwindow xid)
  (time :ulong)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int))

(cffi:defbitfield modifiers
  (:shift 1)
  (:caps-lock 2)
  (:control 4)
  (:alt 8)           ; MOD1
  (:num-lock 16)     ; MOD2
  (:scroll-lock 32)  ; MOD3
  (:super 64)        ; MOD4
  (:hyper 128))      ; MOD5

(cffi:defcstruct (key-event :conc-name key-event-)
  (_ (:struct positioned-event))
  (state modifiers)
  (keycode :uint)
  (same-screen :bool))

(cffi:defcstruct (button-event :conc-name button-event-)
  (_ (:struct positioned-event))
  (state modifiers)
  (button :uint)
  (same-screen :bool))

(cffi:defcstruct (motion-event :conc-name motion-event-)
  (_ (:struct positioned-event))
  (state :uint)
  (is-hint :char)
  (same-screen :bool))

(cffi:defcstruct (focus-change-event :conc-name focus-change-event-)
  (_ (:struct base-event))
  (mode :int)
  (detail :int))

(cffi:defcstruct (configure-event :conc-name configure-event-)
  (_ (:struct base-event))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (border-width :int)
  (above xid)
  (override-redirect :bool))

(cffi:defcstruct (property-event :conc-name property-event-)
  (_ (:struct base-event))
  (atom atom)
  (time :ulong)
  (state :int))

(cffi:defcstruct (destroy-window-event :conc-name destroy-window-event-)
  (_ (:struct base-event)))

(cffi:defcstruct (client-message-event :conc-name client-message-event-)
  (_ (:struct base-event))
  (message-type atom)
  (format :int)
  (data :char :count 20))

(cffi:defcstruct (net-message-event :conc-name net-message-event-)
  (_ (:struct base-event))
  (message-type atom)
  (format :int)
  (protocol atom))

(cffi:defcfun (black-pixel "XBlackPixel") :ulong
  (display :pointer)
  (screen :int))

(cffi:defcfun (default-depth "XDefaultDepth") :int
  (display :pointer)
  (screen :int))

(cffi:defcfun (default-gc "XDefaultGC") :pointer
  (display :pointer)
  (screen :int))

(cffi:defcfun (default-root-window "XDefaultRootWindow") xid
  (display :pointer))

(cffi:defcfun (default-visual "XDefaultVisual") :pointer
  (display :pointer)
  (screen :int))

(cffi:defcfun (default-screen "XDefaultScreen") :int
  (display :pointer))

(cffi:defcfun (display-height "XDisplayHeight") :int
  (display :pointer)
  (screen :int))

(cffi:defcfun (display-width "XDisplayWidth") :int
  (display :pointer)
  (screen :int))

(cffi:defcfun (init-threads "XInitThreads") :int)

(cffi:defcfun (free-threads "XFreeThreads") :int)

(cffi:defcfun (change-property "XChangeProperty") :int
  (display :pointer)
  (window xid)
  (property atom)
  (type atom)
  (format :int)
  (code :int)
  (data :pointer)
  (elements :int))

(cffi:defcfun (get-window-property "XGetWindowProperty") :int
  (display :pointer)
  (window xid)
  (property atom)
  (offset :long)
  (length :long)
  (delete :bool)
  (request-type atom)
  (actual-type :pointer)
  (actual-format :pointer)
  (item-count :pointer)
  (bytes-after-return :pointer)
  (return :pointer))

(cffi:defcfun (clear-window "XClearWindow") :int
  (display :pointer)
  (window xid))

(cffi:defcfun (clear-area "XClearArea") :int
  (display :pointer)
  (window xid)
  (x :int)
  (y :int)
  (width :uint)
  (height :uint)
  (exposures :bool))

(cffi:defcfun (close-display "XCloseDisplay") :int
  (display :pointer))

(cffi:defcfun (create-image "XCreateImage") :pointer
  (display :pointer)
  (visual :pointer)
  (depth :uint)
  (format :int)
  (offset :int)
  (data :pointer)
  (width :uint)
  (height :uint)
  (bitmap-pad :int)
  (bytes-per-line :int))

(cffi:defbitfield window-value-mask
  (:back-pixmap 1)
  :back-pixel
  :border-pixmap
  :border-pixel
  :bit-gravity
  :win-gravity
  :backing-store
  :backing-planes
  :backing-pixel
  :override-redirect
  :save-under
  :event-mask
  :dont-propagate
  :colormap
  :cursor)

(cffi:defcfun (create-window "XCreateWindow") xid
  (display :pointer)
  (window xid)
  (x :int)
  (y :int)
  (width :uint)
  (height :uint)
  (border-width :uint)
  (depth :int)
  (class :uint)
  (visual :pointer)
  (value-mask window-value-mask)
  (attributes :pointer))

(cffi:defcfun (destroy-image "XDestroyImage") :int
  (image :pointer))

(cffi:defcfun (destroy-window "XDestroyWindow") :int
  (display :pointer)
  (window xid))

(cffi:defcfun (events-queued "XEventsQueued") :int
  (display :pointer)
  (mode :int))

(cffi:defcfun (flush "XFlush") :int
  (display :pointer))

(cffi:defcfun (free "XFree") :int
  (data :pointer))

(cffi:defcfun (intern-atom "XInternAtom") atom
  (display :pointer)
  (name :string)
  (if-exists :bool))

(cffi:defcfun (list-pixmap-formats "XListPixmapFormats") :pointer
  (display :pointer)
  (count :pointer))

(cffi:defcfun (lookup-string "XLookupString") :int
  (key-event :pointer)
  (buffer-return :pointer)
  (bytes :int)
  (keysym-return :pointer)
  (status-in-out :pointer))

(cffi:defcfun (map-raised "XMapRaised") :int
  (display :pointer)
  (window xid))

(cffi:defcfun (unmap-window "XUnmapWindow") :int
  (display :pointer)
  (window xid))

(cffi:defcfun (next-event "XNextEvent") :int
  (display :pointer)
  (event :pointer))

(cffi:defcfun (open-display "XOpenDisplay") :pointer
  (name :string))

(cffi:defcfun (peek-event "XPeekEvent") :int
  (display :pointer)
  (event :pointer))

(cffi:defcfun (pending "XPending") :int
  (display :pointer))

(cffi:defcfun (put-image "XPutImage") :int
  (display :pointer)
  (drawable xid)
  (gc :pointer)
  (image :pointer)
  (x :int)
  (y :int)
  (dst-x :int)
  (dst-y :int)
  (width :uint)
  (height :uint))

(cffi:defcenum (event-mask :long)
  (:key-press 0)
  :key-release
  :button-press
  :button-release
  :enter-window
  :leave-window
  :pointer-motion
  :pointer-motion-hint
  :button1-motion
  :button2-motion
  :button3-motion
  :button4-motion
  :button5-motion
  :button-motion
  :keymap-state
  :exposure
  :visibility-change
  :structure-notify
  :resize-redirect
  :substructure-notify
  :substructure-redirect
  :focus-change
  :property-change
  :colormap-change
  :owner-grab-button)

(cffi:defcfun (select-input "XSelectInput") :int
  (display :pointer)
  (window xid)
  (event-mask event-mask))

(cffi:defcfun (set-wm-normal-hints "XSetWMNormalHints") :void
  (display :pointer)
  (window xid)
  (hints :pointer))

(cffi:defcfun (set-wm-protocols "XSetWMProtocols") :int
  (display :pointer)
  (window xid)
  (protocols :pointer)
  (count :int))

(cffi:defcfun (store-name "XStoreName") :int
  (display :pointer)
  (window xid)
  (name :string))

(cffi:defcfun (move-window "XMoveWindow") :int
  (display :pointer)
  (window xid)
  (x :int)
  (y :int))

(cffi:defcfun (resize-window "XResizeWindow") :int
  (display :pointer)
  (window xid)
  (width :uint)
  (height :uint))

(cffi:defcfun (send-event "XSendEvent") :int
  (display :pointer)
  (window xid)
  (propagate :bool)
  (event-mask :long)
  (event :pointer))

(cffi:defcfun (iconify-window "XIconifyWindow") :int
  (display :pointer)
  (window xid)
  (screen :int))

;; XKB
(cffi:defcfun (xkb-keycode-to-keysym "XkbKeycodeToKeysym") xid
  (display :pointer)
  (code :uchar)
  (group :int)
  (level :int))

;; XSHM
(cffi:defcstruct shm-segment-into
  (message :ulong)
  (id :int)
  (address :pointer)
  (read-only :pointer))

(cffi:defcfun (xshm-query-extension "XShmQueryExtension") :bool
  (display :pointer))

(cffi:defcfun (xshm-pixmap-format "XShmPixmapFormat") :int
  (display :pointer))

(cffi:defcfun (xshm-put-image "XShmPutImage") :bool
  (display :pointer)
  (drawable xid)
  (gc :pointer)
  (image :pointer)
  (x :int)
  (y :int)
  (dst-x :int)
  (dst-y :int)
  (width :uint)
  (height :uint)
  (send-event :bool))

(cffi:defcfun (xshm-create-image "XShmCreateImage") :pointer
  (display :pointer)
  (visual :pointer)
  (depth :uint)
  (format :int)
  (data :pointer)
  (shm-info :pointer)
  (width :uint)
  (height :uint))
