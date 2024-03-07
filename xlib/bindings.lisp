(in-package #:org.shirakumo.framebuffers.xlib.cffi)

(cffi:define-foreign-library x11
  (:defaults "X11"))

(cffi:defctype xid :ulong)
(cffi:defctype atom :ulong)

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

(cffi:defcstruct (base-event :conc-name base-event-)
  (type :int)
  (serial :ulong)
  (send-event :bool)
  (display :pointer)
  (window xid))

(cffi:defcstruct (positioned-event :conc-name positioned-event-)
  (_ (:struct base-event))
  (root xid)
  (subwindow xid)
  (time time)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int))

(cffi:defcstruct (key-event :conc-name key-event-)
  (_ (:struct positioned-event))
  (state :uint)
  (keycode :unit)
  (same-screen :bool))

(cffi:defcstruct (button-event :conc-name button-event-)
  (_ (:struct positioned-event))
  (state :uint)
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

(cffi:defcstruct (destroy-window-event :conc-name destroy-window-event-)
  (_ (:struct base-event)))

(cffi:defcstruct (client-message-event :conc-name client-message-event-)
  (_ (:struct base-event))
  (message-type atom)
  (format :int)
  (data :char :count 20))

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
  (:back-pixmap 0)
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
