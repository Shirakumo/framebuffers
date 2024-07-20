(in-package #:org.shirakumo.framebuffers.xlib)
(pushnew :xlib fb-int:*available-backends*)

(define-condition xlib-error (fb:framebuffer-error)
  ((code :initarg :code :initform NIL :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "An X11 call failed~@[ (~a)~]~@[:~%  ~a~]"
                                 (code c) (message c)))))

(cffi:defcallback error-handler :int ((display :pointer) (event :pointer))
  (cffi:with-foreign-objects ((str :char 256))
    (xlib:get-error-text display (xlib:error-event-error-code event) str 256)
    (error 'xlib-error :window (fb-int:ptr-window display)
                       :code (xlib:error-event-error-code event)
                       :message (cffi:mem-ref str :string))))

(cffi:defcallback io-error-handler :int ((display :pointer))
  (let ((window (fb-int:ptr-window display)))
    ;; Invalidate the window
    (when window
      (setf (fb-int:ptr-window display) NIL)
      (setf (xid window) NIL)
      (setf (image window) NIL)
      (setf (display window) NIL))
    (error 'xlib-error :window window
                       :message "An IO error occurred and the display connection has been closed.")))

(cffi:defcallback io-error-exit-handler :void ((display :pointer) (user :pointer))
  (declare (ignore user))
  (let ((window (fb-int:ptr-window display)))
    (when window
      (setf (fb-int:ptr-window display) NIL)
      (setf (xid window) NIL)
      (setf (image window) NIL)
      (setf (display window) NIL))))

(defun check-pixmap-formats (display)
  (let* ((screen (xlib:default-screen display))
         (depth (xlib:default-depth display screen)))
    (unless (cffi:with-foreign-objects ((count :int))
              (let ((array (xlib:list-pixmap-formats display count)))
                (unwind-protect (dotimes (i (cffi:mem-ref count :int))
                                  (let ((format (cffi:mem-aptr array '(:struct xlib:pixmap-format) i)))
                                    (when (and (= depth (xlib:pixmap-format-depth format))
                                               (= 32 (xlib:pixmap-format-bits-per-pixel format)))
                                      (return T))))
                  (xlib:free array))))
      (error "Server does not support 32bpp format."))))

(defvar *global-display* NIL)
(defvar *init* NIL)

(defmethod fb-int:init-backend ((backend (eql :xlib)))
  (unless *init*
    (unless (cffi:foreign-library-loaded-p 'xlib:x11)
      (cffi:use-foreign-library xlib:x11))
    (unless (cffi:foreign-library-loaded-p 'xlib:xext)
      (ignore-errors (cffi:use-foreign-library xlib:xext)))
    (unless (cffi:foreign-library-loaded-p 'xlib:xrandr)
      (ignore-errors (cffi:use-foreign-library xlib:xrandr)))
    (unless (cffi:foreign-library-loaded-p 'xlib:xcursor)
      (ignore-errors (cffi:use-foreign-library xlib:xcursor)))
    (unless (cffi:foreign-library-loaded-p 'xlib:xinerama)
      (ignore-errors (cffi:use-foreign-library xlib:xinerama)))
    (unless (cffi:foreign-library-loaded-p 'xlib:xi)
      (ignore-errors (cffi:use-foreign-library xlib:xi)))
    (let ((display (xlib:open-display (cffi:null-pointer))))
      (when (cffi:null-pointer-p display)
        (error "Failed to open display."))
      (fb-int:with-cleanup (xlib:close-display display)
        (check-pixmap-formats display)
        (setf *global-display* display))
      (cffi:with-foreign-objects ((event-base :int) (error-base :int))
        (unless (xlib:xrr-query-extension display event-base error-base)
          (cffi:close-foreign-library 'xlib:xrandr))
        (unless (xlib:xinerama-query-extension display event-base error-base)
          (cffi:close-foreign-library 'xlib:xinerama))))
    (xlib:init-threads)
    (setf *init* T)
    (ignore-errors
     (xlib:set-error-handler (cffi:callback error-handler)))
    (ignore-errors
     (xlib:set-io-error-handler (cffi:callback io-error-handler)))))

(defmethod fb-int:shutdown-backend ((backend (eql :xlib)))
  (when *init*
    (when *global-display*
      (xlib:close-display *global-display*)
      (setf *global-display* NIL))
    (xlib:free-threads)
    (setf *init* NIL)))

(defun check-create (result &key call)
  (if (etypecase result
        (cffi:foreign-pointer (cffi:null-pointer-p result))
        (integer (= 0 result)))
      (if call
          (error "Failed to ~a" call)
          (error "Failed to create X11 object"))
      result))

(defmacro with-creation ((var creator) cleanup &body body)
  (let ((ok (gensym "OK")))
    `(let ((,ok NIL)
           (,var (check-create ,creator :call ',(car creator))))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (setf ,ok T))
         (unless ,ok
           ,cleanup)))))

(defun content-scale (display)
  (let ((xdpi 96) (ydpi 96)
        (rms (xlib:resource-manager-string display)))
    (unless (cffi:null-pointer-p rms)
      (let ((db (xlib:xrm-get-string-database rms)))
        (unless (cffi:null-pointer-p db)
          (cffi:with-foreign-objects ((value '(:struct xlib:xrm-value))
                                      (type :pointer))
            (when (xlib:xrm-get-resource db "Xft.dpi" "Xft.Dpi" type value)
              (when (and (not (cffi:null-pointer-p (cffi:mem-ref type :pointer)))
                         (string= "String" (cffi:foreign-string-to-lisp (cffi:mem-ref type :pointer))))
                (setf xdpi (cffi:foreign-funcall "atof" :pointer (xlib:xrm-value-addr value) :double))
                (setf ydpi xdpi)))
            (xlib:xrm-destroy-database db)))))
    (cons (/ xdpi 96) (/ ydpi 96))))

(defun probe-xkb (display)
  (cffi:with-foreign-objects ((opcode :int)
                              (event-base :int)
                              (error-base :int)
                              (major :int)
                              (minor :int))
    (xlib:xkb-query-extension display opcode event-base error-base major minor)))

(defmethod fb-int:open-backend ((backend (eql :xlib)) &key (size (cons NIL NIL)) (location (cons NIL NIL)) (title (fb-int:default-title)) (visible-p T) event-handler display)
  (with-creation (display (xlib:open-display (or display (cffi:null-pointer)))) (xlib:close-display display)
    (ignore-errors
     (xlib:set-io-error-exit-handler display (cffi:callback io-error-exit-handler) (cffi:null-pointer)))
    (unless *keytable*
      (init-keytable display (probe-xkb display)))
    (let* ((screen (xlib:default-screen display))
           (visual (xlib:default-visual display screen))
           (depth (xlib:default-depth display screen)))
      ;; Default size and location
      (unless (car size) (setf (car size) (xlib:display-width display screen)))
      (unless (cdr size) (setf (cdr size) (xlib:display-height display screen)))
      (unless (car location) (setf (car location) (truncate (- (xlib:display-width display screen) (car size)) 2)))
      (unless (cdr location) (setf (cdr location) (truncate (- (xlib:display-height display screen) (cdr size)) 2)))
      (cffi:with-foreign-objects ((attrs '(:struct xlib:set-window-attributes))
                                  (protos 'xlib:atom 10))
        (setf (xlib:set-window-attributes-border-pixel attrs) (xlib:black-pixel display screen))
        (setf (xlib:set-window-attributes-background-pixel attrs) (xlib:black-pixel display screen))
        (setf (xlib:set-window-attributes-backing-store attrs) 0)
        (with-creation (window (xlib:create-window display (xlib:default-root-window display)
                                                   (car location) (cdr location) (car size) (cdr size)
                                                   0 depth 1 visual '(:back-pixel :border-pixel :backing-store) attrs))
                       (xlib:destroy-window display window)
          (xlib:store-name display window title)
          (xlib:select-input display window '(:key-press :key-release :button-press :button-release :pointer-motion
                                              :structure-notify :exposure :focus-change :enter-window :leave-window))
          (setf (cffi:mem-aref protos 'xlib:atom 0) (xlib:intern-atom display "WM_DELETE_WINDOW" 0))
          (setf (cffi:mem-aref protos 'xlib:atom 1) (xlib:intern-atom display "NET_WM_PING" 0))
          (xlib:set-wm-protocols display window protos 2)
          (xlib:clear-window display window)
          (when visible-p
            (xlib:map-raised display window))
          (xlib:flush display)
          (make-instance 'window :display display :xid window :screen screen
                                 :size size :location location :title title :visible-p visible-p
                                 :content-scale (content-scale display)
                                 :event-handler event-handler))))))

(defclass window (#+linux fb-int::linux-window fb:window)
  ((display :initarg :display :accessor display)
   (screen :initarg :screen :accessor screen)
   (xid :initarg :xid :accessor xid)
   (gc :initarg :gc :initform NIL :accessor gc)
   (xkb :initarg :xkb :initform NIL :accessor xkb)
   (xshm :initarg :xshm :initform NIL :accessor xshm)
   (image :initarg :image :initform NIL :accessor image)
   (buffer :initarg :buffer :initform NIL :reader fb:buffer :accessor buffer)
   (atom-table :initform (make-hash-table :test 'equal) :reader atom-table)))

(defmethod initialize-instance :after ((window window) &key)
  (setf (fb-int:ptr-window (display window)) window)
  (unless (xkb window) (setf (xkb window) (probe-xkb (display window))))
  #+(and static-vectors-aligned framebuffers-use-xshm)
  (unless (xshm window) (when (ignore-errors (xlib:xshm-query-extension (display window)))
                          (cffi:foreign-alloc '(:struct xlib:shm-segment-info))))
  (unless (gc window) (setf (gc window) (xlib:default-gc (display window) (screen window))))
  (unless (buffer window) (update-buffer window (car (fb:size window)) (cdr (fb:size window)))))

(defun update-buffer (window w h)
  (when (image window)
    (setf (xlib:image-data (image window)) (cffi:null-pointer))
    (xlib:destroy-image (image window)))
  (let* ((buffer (fb-int:resize-buffer w h (buffer window) (car (fb:size window)) (cdr (fb:size window))))
         (depth (xlib:default-depth (display window) (screen window)))
         (image (cond ((xshm window)
                       ;; FIXME: This does not seem to work, shmat won't attach to the static vector pointer, probably due to alignment problems.
                       (let* ((shm (xshm window))
                              (image (check-create (xlib:xshm-create-image (display window) (cffi:null-pointer) depth 2 (cffi:null-pointer) shm w h)))
                              (id (cffi:foreign-funcall "shmget" :int 0 :size (* h (xlib:image-bytes-per-line image)) :int #x1777 :int))
                              (ptr (cffi:foreign-funcall "shmat" :int id :pointer (static-vectors:static-vector-pointer buffer) :int 0 :pointer)))
                         (when (= id -1)
                           (error 'xlib-error :message "Failed to create shared memory region." :window window))
                         (when (= (cffi:pointer-address ptr) (1- (ash 1 64)))
                           (error 'xlib-error :message "Failed to bind shared memory region." :window window))
                         (setf (xlib:shm-segment-info-id shm) id)
                         (setf (xlib:shm-segment-info-address shm) ptr)
                         (setf (xlib:shm-segment-info-read-only shm) T)
                         (unless (xlib:xshm-attach (display window) shm)
                           (error 'xlib-error :message "Failed to attach shared memory." :window window))
                         image))
                      (T
                       (check-create (xlib:create-image (display window) (cffi:null-pointer) depth 2 0 (cffi:null-pointer) w h 32 (* 4 w)))))))
    (setf (buffer window) buffer)
    (setf (image window) image)
    (setf (car (fb:size window)) w)
    (setf (cdr (fb:size window)) h)
    window))

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (let ((display (display window))
        (image (image window)))
    (cond ((xshm window)
           (xlib:xshm-put-image display (xid window) (gc window) image x y x y w h NIL))
          #+static-vectors
          (T
           (setf (xlib:image-data image) (static-vectors:static-vector-pointer (buffer window)))
           (xlib:put-image display (xid window) (gc window) image x y x y w h))
          #-static-vectors
          (T
           (cffi:with-pointer-to-vector-data (ptr (buffer window))
             (setf (xlib:image-data image) ptr)
             (xlib:put-image display (xid window) (gc window) image x y x y w h))))
    (if sync
        (xlib:sync display)
        (xlib:flush display))
    window))

(defun atom (window name)
  (etypecase name
    ((or symbol string)
     (let ((name (string name)))
       (or (gethash name (atom-table window))
           (setf (gethash name (atom-table window)) (xlib:intern-atom (display window) name 0)))))
    ((integer 0)
     name)))

(defun atomp (window atom name)
  (= atom (atom window name)))

(defun send-client-event (window type a b c d e)
  (cffi:with-foreign-objects ((event '(:struct xlib:client-message-event)))
    (setf (xlib:base-event-type event) :client-message)
    (setf (xlib:base-event-window event) (xid window))
    (setf (xlib:client-message-event-message-type event) (atom window type))
    (setf (xlib:client-message-event-format event) 32)
    (let ((ptr (cffi:foreign-slot-pointer event '(:struct xlib:client-message-event) 'xlib::data)))
      (setf (cffi:mem-aref ptr :int32 0) a)
      (setf (cffi:mem-aref ptr :int32 1) b)
      (setf (cffi:mem-aref ptr :int32 2) c)
      (setf (cffi:mem-aref ptr :int32 3) d)
      (setf (cffi:mem-aref ptr :int32 4) e))
    (xlib:send-event (display window) (xlib:default-root-window (display window))
                     NIL 1572864 event)))

(defun get-property (window property type value)
  (cffi:with-foreign-objects ((actual-type 'xlib:atom)
                              (actual-format :int)
                              (item-count :ulong)
                              (bytes-after :ulong))
    (xlib:get-window-property (display window) (xid window) (atom window property) 0 2147483647 NIL (atom window type)
                              actual-type actual-format item-count bytes-after value)
    (cffi:mem-ref item-count :ulong)))

(defun get-state (window)
  (cffi:with-foreign-objects ((state :pointer))
    (prog1 (if (<= 2 (get-property window "WM_STATE" "WM_STATE" state))
               (ecase (cffi:mem-ref (cffi:mem-ref state :pointer) :uint32)
                 (0 :withdrawn)
                 (1 :normal)
                 (2 :iconic))
               :withdrawn)
      (xlib:free (cffi:mem-ref state :pointer)))))

(defmethod fb:valid-p ((window window))
  (not (null (xid window))))

(defmethod fb:close ((window window))
  (when (xshm window)
    (cffi:foreign-free (xshm window))
    (setf (xshm window) NIL))
  (when (image window)
    (setf (xlib:image-data (image window)) (cffi:null-pointer))
    (xlib:destroy-image (image window))
    (setf (image window) NIL))
  (when (buffer window)
    #+static-vectors
    (static-vectors:free-static-vector (buffer window))
    (setf (buffer window) NIL))
  (when (xid window)
    (xlib:destroy-window (display window) (xid window))
    (setf (xid window) NIL))
  (when (display window)
    (setf (fb-int:ptr-window (display window)) NIL)
    (xlib:close-display (display window))
    (setf (display window) NIL))
  window)

(defmethod (setf fb:size) (size (window window))
  (destructuring-bind (w . h) size
    (xlib:resize-window (display window) (xid window) w h)
    (xlib:flush (display window))
    size))

(defmethod (setf fb:location) (location (window window))
  (destructuring-bind (x . y) location
    (xlib:move-window (display window) (xid window) x y)
    (xlib:flush (display window))
    location))

(defmethod (setf fb:title) (title (window window))
  (xlib:store-name (display window) (xid window) title)
  (xlib:flush (display window))
  title)

(defmethod (setf fb:visible-p) (state (window window))
  (if state
      (xlib:map-raised (display window) (xid window))
      (xlib:unmap-window (display window) (xid window)))
  (xlib:flush (display window))
  state)

(defmethod (setf fb:maximized-p) (state (window window))
  (cond (state
         (cond ((fb:visible-p window)
                (send-client-event window "NET_WM_STATE" 1 
                                   (atom window "NET_WM_STATE_MAXIMIZED_VERT")
                                   (atom window "NET_WM_STATE_MAXIMIZED_HORZ")
                                   1 0))
               (T
                (let ((count 2))
                  (cffi:with-foreign-objects ((missing 'xlib:atom 2)
                                              (states :pointer))
                    (setf (cffi:mem-aref missing 'xlib:atom 0) (atom window "NET_WM_STATE_MAXIMIZED_VERT"))
                    (setf (cffi:mem-aref missing 'xlib:atom 1) (atom window "NET_WM_STATE_MAXIMIZED_HORZ"))
                    (dotimes (i (get-property window "NET_WM_STATE" 4 states))
                      (dotimes (j 2)
                        (when (= (cffi:mem-aref missing 'xlib:atom j) (cffi:mem-aref (cffi:mem-ref states :pointer) 'xlib:atom i))
                          (setf (cffi:mem-aref missing 'xlib:atom j) (cffi:mem-aref missing 'xlib:atom (1- count)))
                          (decf count))))
                    (xlib:free (cffi:mem-ref states :pointer))
                    (when (< 0 count)
                      (xlib:change-property (display window) (xid window)
                                            (atom window "NET_WM_STATE") 4 32 2 missing count)))))))
        ((fb:maximized-p window)
         (send-client-event window "NET_WM_STATE" 0
                            (atom window "NET_WM_STATE_MAXIMIZED_VERT")
                            (atom window "NET_WM_STATE_MAXIMIZED_HORZ")
                            1 0)))
  (xlib:flush (display window))
  (setf (fb-int:maximized-p window) state))

(defmethod (setf fb:iconified-p) (state (window window))
  (cond (state
         (xlib:iconify-window (display window) (xid window) (screen window))
         (xlib:flush (display window))
         (setf (fb-int:iconified-p window) T))
        (T
         (setf (fb:visible-p window) T)
         (setf (fb-int:iconified-p window) NIL))))

(defun update-size-hints (window)
  (cffi:with-foreign-objects ((hint '(:struct xlib:size-hint)))
    (cffi:foreign-funcall "memset" :pointer hint :int 0 :size (cffi:foreign-type-size '(:struct xlib:size-hint)))
    (setf (xlib:size-hint-flags hint) '(:min-size :max-size))
    (cond ((fb:resizable-p window)
           (destructuring-bind (-x . -y) (fb:minimum-size window)
             (destructuring-bind (+x . +y) (fb:maximum-size window)
               (setf (xlib:size-hint-min-width hint) (max -x 1))
               (setf (xlib:size-hint-max-width hint) (min +x (1- (ash 1 16))))
               (setf (xlib:size-hint-min-height hint) (max -y 1))
               (setf (xlib:size-hint-max-height hint) (min +y (1- (ash 1 16)))))))
          (T
           (destructuring-bind (x . y) (fb:size window)
             (setf (xlib:size-hint-min-width hint) x)
             (setf (xlib:size-hint-max-width hint) x)
             (setf (xlib:size-hint-width hint) x)
             (setf (xlib:size-hint-min-height hint) y)
             (setf (xlib:size-hint-max-height hint) y)
             (setf (xlib:size-hint-height hint) y))))
    (xlib:set-wm-normal-hints (display window) (xid window) hint)
    (xlib:flush (display window))))

(defmethod (setf fb:minimum-size) (value (window window))
  (setf (car (fb:minimum-size window)) (or (car value) 1))
  (setf (cdr (fb:minimum-size window)) (or (cdr value) 1))
  (update-size-hints window)
  value)

(defmethod (setf fb:maximum-size) (value (window window))
  (setf (car (fb:maximum-size window)) (car value))
  (setf (cdr (fb:maximum-size window)) (cdr value))
  (update-size-hints window)
  value)

(defmethod (setf fb:focused-p) (value (window window))
  (when value
    (send-client-event window "NET_ACTIVE_WINDOW" 1 0 0 0 0))
  value)

(defmethod (setf fb:borderless-p) (value (window window))
  (cffi:with-foreign-objects ((hints :long 5))
    (setf (cffi:mem-ref hints :ulong 0) 2) ;; Decorations
    (setf (cffi:mem-ref hints :ulong 1) 0)
    (setf (cffi:mem-ref hints :ulong 2) (if value 1 0))
    (setf (cffi:mem-ref hints :ulong 3) 0)
    (setf (cffi:mem-ref hints :ulong 4) 0)
    (xlib:change-property (display window) (xid window)
                          (atom window "MOTIF_WM_HINTS") (atom window "MOTIF_WM_HINTS")
                          32 2 hints 5)
    (setf (fb-int:borderless-p window) value)))

(defmethod (setf fb:always-on-top-p) (value (window window))
  ;; Not available.
  value)

(defmethod (setf fb:resizable-p) (value (window window))
  (setf (fb-int:resizable-p window) value)
  (update-size-hints window)
  value)

(defmethod (setf fb:floating-p) (value (window window))
  (cond ((fb:visible-p window)
         (send-client-event window "NET_WM_STATE" (if value 1 0) (atom window "NET_WM_STATE_ABOVE") 0 1 0))
        (T
         (cffi:with-foreign-objects ((states :pointer))
           (let ((count (get-property window "WM_STATE" "XA_ATOM" states)))
             (if (loop for i from 0 below count
                       for state = (cffi:mem-aref states 'xlib:atom i)
                       do (when (= state (atom window "NET_WM_STATE_ABOVE"))
                            (setf (cffi:mem-aref states 'xlib:atom i) (cffi:mem-aref states 'xlib:atom (1- count)))
                            (return T)))
                 (unless value
                   (xlib:change-property (display window) (xid window) (atom window "NET_WM_STATE")
                                         4 32 0 states (1- count)))
                 (when value
                   (setf (cffi:mem-ref states 'xlib:atom 0) (atom window "NET_WM_STATE_ABOVE"))
                   (xlib:change-property (display window) (xid window) (atom window "NET_WM_STATE")
                                         4 32 2 states 1)))))))
  (setf (fb-int:floating-p window) value))

(defmethod fb:clipboard ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard) ((string string) (window window))
  ;; TODO: implement clipboard setting
  )

(defmethod (setf fb:icon) ((value null) (window window))
  (xlib:delete-property (display window) (xid window) (atom window "NET_WM_ICON"))
  (xlib:flush (display window))
  (setf (fb-int:icon window) value)
  value)

(defmethod (setf fb:icon) ((value fb:icon) (window window))
  (let ((longcount (+ 2 (* (fb:width value) (fb:height value))))
        (buffer (fb:buffer value)))
    (cffi:with-foreign-objects ((icon :ulong longcount))
      (setf (cffi:mem-aref icon :ulong 0) (fb:width value))
      (setf (cffi:mem-aref icon :ulong 1) (fb:height value))
      (loop for i from 0 below (length buffer) by 4
            for j from 2
            do (setf (ldb (byte 8  0) (cffi:mem-aref icon :ulong j)) (aref buffer (+ i 0)))
               (setf (ldb (byte 8  8) (cffi:mem-aref icon :ulong j)) (aref buffer (+ i 1)))
               (setf (ldb (byte 8 16) (cffi:mem-aref icon :ulong j)) (aref buffer (+ i 2)))
               (setf (ldb (byte 8 24) (cffi:mem-aref icon :ulong j)) (aref buffer (+ i 3))))
      (xlib:change-property (display window) (xid window) (atom window "NET_WM_ICON")
                            6 32 2 icon longcount)))
  (xlib:flush (display window))
  (setf (fb-int:icon window) value)
  value)

(defmethod (setf fb:cursor-icon) (value (window window))
  ;; TODO: implement cursor-icon
  )

(defmethod (setf fb:cursor-state) (value (window window))
  ;; TODO: implement cursor-state
  )

(defmethod fb:request-attention ((window window))
  (send-client-event window "NET_WM_STATE" 1 (atom window "NET_WM_STATE_DEMANDS_ATTENTION") 0 1 0)
  (xlib:flush (display window))
  window)

(defmethod fb:key-scan-code ((key integer) (window window))
  (key-code key))

(defmethod fb:local-key-string ((key integer) (window window))
  (keysym-string key))

(defvar *displays* ())

(defclass display (fb-int:display)
  ((crtc :initarg :crtc :initform NIL :accessor crtc)
   (default-mode :initform NIL :accessor default-mode)
   (xinerama :initarg :xinerama :initform NIL :accessor xinerama)))

(defstruct (video-mode (:include fb:video-mode))
  mode-id)

(defun list-video-modes (sr ci oi)
  (loop for i from 0 below (xlib:output-info-mode-count oi)
        for mi = (dotimes (j (xlib:screen-resources-mode-count sr))
                   (let ((mode (cffi:mem-aptr (xlib:screen-resources-modes sr) '(:struct xlib:mode-info) j)))
                     (when (= (xlib:mode-info-id mode) (cffi:mem-aref (xlib:output-info-modes oi) :int i))
                       (return mode))))
        unless (or (null mi) (member :interlace (xlib:mode-info-flags mi)))
        collect (let ((w (xlib:mode-info-width mi))
                      (h (xlib:mode-info-height mi))
                      (ht (xlib:mode-info-htotal mi))
                      (vt (xlib:mode-info-vtotal mi)))
                  (when (or (member :90 (xlib:crtc-info-rotation ci))
                            (member :270 (xlib:crtc-info-rotation ci)))
                    (rotatef w h))
                  (make-video-mode :width w :height h :refresh-rate
                                   (if (and (< 0 vt) (< 0 ht))
                                       (round (/ (xlib:mode-info-dot-clock mi) ht vt))
                                       60)
                                   :mode-id (xlib:mode-info-id mi)))))

(defun poll-xrandr (&optional (display *global-display*) window)
  ;; First, gather a list of all displays
  (let ((sr (xlib:xrr-get-screen-resources-current display (xlib:default-root-window display)))
        (primary (xlib:xrr-get-output-primary display (xlib:default-root-window display)))
        (screens (when (cffi:foreign-library-loaded-p 'xlib:xinerama)
                   (cffi:with-foreign-objects ((count :int))
                     (cons (xlib:xinerama-query-screens display count)
                           (cffi:mem-ref count :int)))))
        (found ()))
    (dotimes (i (xlib:screen-resources-output-count sr))
      (let* ((out (cffi:mem-aref (xlib:screen-resources-outputs sr) 'xlib:xid i))
             (oi (xlib:xrr-get-output-info display sr out)))
        (unless (or (not (eq :connected (xlib:output-info-connection oi)))
                    (= 0 (xlib:output-info-crtc oi)))
          (let* ((ci (xlib:xrr-get-crtc-info display sr (xlib:output-info-crtc oi)))
                 (modes (delete-duplicates (list-video-modes sr ci oi)
                                           :test #'string= :key #'fb:id)))
            (push (list out
                        :crtc (xlib:output-info-crtc oi)
                        :title (xlib:output-info-name oi)
                        :xinerama (dotimes (i (cdr screens))
                                    (let ((scr (cffi:mem-aptr (car screens) '(:struct xlib:screen) i)))
                                      (when (and (= (xlib:crtc-info-x ci) (xlib:screen-x scr))
                                                 (= (xlib:crtc-info-y ci) (xlib:screen-y scr))
                                                 (= (xlib:crtc-info-width ci) (xlib:screen-width scr))
                                                 (= (xlib:crtc-info-height ci) (xlib:screen-height scr)))
                                        (return i))))
                        :primary-p (= primary out)
                        :location (cons (xlib:crtc-info-x ci) (xlib:crtc-info-y ci))
                        :video-modes modes
                        :video-mode (find (xlib:crtc-info-mode ci) modes :key #'video-mode-mode-id))
                  found)
            (xlib:xrr-free-crtc-info ci)))
        (xlib:xrr-free-output-info oi)))
    (xlib:xrr-free-screen-resources sr)
    (when screens (xlib:free (car screens)))
    ;; Next, diff it and update
    (loop for cons on found
          for data = (car cons)
          for display = (find (car data) *displays* :key #'fb:id)
          do (cond (display
                    (apply #'reinitialize-instance display :id data))
                   (T
                    (setf display (apply #'make-instance 'display :id data))
                    (when window (fb:display-connected window display T))))
             (setf (car cons) display))
    (loop for display in *displays*
          do (unless (find display found)
               (when window (fb:display-connected window display NIL))))
    (setf *displays* found)))

(defmethod fb-int:list-displays-backend ((backend (eql :xlib)))
  (or *displays*
      (cond ((cffi:foreign-library-loaded-p 'xlib:xrandr)
             (setf *displays* (poll-xrandr *global-display*)))
            (T
             (let* ((w (xlib:display-width *global-display* (xlib:default-screen *global-display*)))
                    (h (xlib:display-height *global-display* (xlib:default-screen *global-display*)))
                    (mode (fb-int:make-video-mode NIL w h 60))
                    (display (make-instance 'display
                                            :id "-"
                                            :title "Display"
                                            :size (cons w h)
                                            :primary-p T
                                            :video-mode mode
                                            :video-modes (list mode))))
               (setf *displays* (list display)))))))

(defmethod fb:display ((window window))
  (etypecase (fb:fullscreen-p window)
    (fb:video-mode (fb:display (fb:fullscreen-p window)))
    (fb:display (fb:fullscreen-p window))
    (null (call-next-method))))

(defun activate-mode (mode &optional (display *global-display*))
  (let* ((sr (xlib:xrr-get-screen-resources-current display (xlib:default-root-window display)))
         (ci (xlib:xrr-get-crtc-info display sr (crtc (fb:display mode)))))
    (xlib:xrr-set-crtc-config display sr (crtc (fb:display mode)) 0
                              (xlib:crtc-info-x ci) (xlib:crtc-info-y ci)
                              (video-mode-mode-id mode)
                              (xlib:crtc-info-rotation ci)
                              (xlib:crtc-info-outputs ci)
                              (xlib:crtc-info-output-count ci))
    (xlib:xrr-free-crtc-info ci)
    (xlib:xrr-free-screen-resources sr)
    (setf (fb-int:video-mode (fb:display mode)) mode)))

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  (when (fb:fullscreen-p window)
    (typecase (fb:fullscreen-p window)
      (display)
      (video-mode
       (activate-mode (default-mode (fb:display (fb:fullscreen-p window))) (display window))))
    ;; Restore old size
    (when (cffi:foreign-library-loaded-p 'xlib:xinerama)
      (xlib:delete-property (display window) (xid window) (atom window "NET_WM_FULLSCREEN_MONITORS")))
    (send-client-event window "NET_WM_STATE" 0 (atom window "NET_WM_STATE_FULLSCREEN") 0 1 0))
  (setf (fb-int:fullscreen-p window) value))

(defmethod (setf fb:fullscreen-p) ((value video-mode) (window window))
  (unless (eq value (fb:fullscreen-p window))
    (cond ((eq value (fb:video-mode (fb:display value)))
           (setf (fb-int:fullscreen-p window) (fb:display value)))
          (T
           (setf (default-mode (fb:display window)) (fb:video-mode (fb:display value)))
           (activate-mode value (display window))
           (setf (fb-int:fullscreen-p window) value)))
    (when (cffi:foreign-library-loaded-p 'xlib:xinerama)
      (let ((xi (xinerama (fb:display value))))
        (send-client-event window "NET_WM_FULLSCREEN_MONITORS" xi xi xi xi 0)))
    (send-client-event window "NET_WM_STATE" 1 (atom window "NET_WM_STATE_FULLSCREEN") 0 1 0)))

(defmethod (setf fb:fullscreen-p) ((value fb:video-mode) (window window))
  ;; Don't have Xrandr, so just fullscreen with resize.
  (setf (fb:size window) (cons (fb:width value) (fb:height value))))

(defmethod fb-int:wait-for-events ((backend (eql :xlib)) windows &key timeout)
  (let ((millis (etypecase timeout
                  (real (truncate (* 1000 timeout)))
                  ((eql T) 1000)
                  (null 0)))
        (fds (loop for window in windows
                   when (display window)
                   append (list* (xlib:connection-number (display window)) (fb-int::timers window))))
        (found ()))
    (loop (dolist (fd (fb-int::poll fds millis))
            (dolist (window windows)
              (when (or (eql fd (xlib:connection-number (display window)))
                        (find fd (fb-int::timers window)))
                (push window found))))
          (when (or found (not (eql T timeout)))
            (return found)))))

(defmethod fb:process-events ((window window) &key timeout)
  (cffi:with-foreign-objects ((event '(:struct xlib:event)))
    (let ((millis (etypecase timeout
                    (real (truncate (* 1000 timeout)))
                    ((eql T) 1000)
                    (null 0))))
      (loop while (and (display window) (not (fb:close-requested-p window)))
            do (dolist (fd (fb-int::poll (list* (xlib:connection-number (display window)) (fb-int::timers window)) millis))
                 (if (= fd (xlib:connection-number (display window)))
                     (loop while (and (display window) (< 0 (xlib:events-queued (display window) 1)))
                           do (xlib:next-event (display window) event)
                              (process-event window (xlib:base-event-type event) event))
                     (fb:timer-triggered window fd)))
               (unless (eql T timeout)
                 (return))))
    window))

(defmethod process-event ((window window) type event)
  (when (cffi:foreign-library-loaded-p 'xlib:xrandr)
    ;; FIXME: this is dumb, but I'm lazy
    (cffi:with-foreign-objects ((event-base :int) (error-base :int))
      (xlib:xrr-query-extension (display window) event-base error-base)
      (when (eql (xlib:base-event-type event) (+ 1 (cffi:mem-ref event-base :int)))
        (xlib:xrr-update-configuration event)
        (poll-xrandr (display window) window)))))

(flet ((process-key-event (window action event)
         (when (and (eql action :release) (< 0 (xlib:events-queued (display window) 1)))
           (cffi:with-foreign-objects ((next '(:struct xlib:event)))
             (xlib:peek-event (display window) next)
             (when (and (eql :key-press (xlib:base-event-type next))
                        (= (xlib:positioned-event-time next) (xlib:positioned-event-time event))
                        (< (- (xlib:key-event-keycode next) (xlib:key-event-keycode event)) 20))
               (xlib:next-event (display window) event)
               (setf action :repeat))))
         (let ((code (xlib:key-event-keycode event)))
           (fb:key-changed window (translate-keycode code) code action (xlib:key-event-state event))
           (case action
             ((:repeat :press)
              (cffi:with-foreign-objects ((keysym :uint32))
                (xlib:lookup-string event (cffi:null-pointer) 0 keysym (cffi:null-pointer))
                (let ((str (keysym-string (cffi:mem-ref keysym :uint32))))
                  (when str (fb:string-entered window str)))))))))

  (defmethod process-event ((window window) (type (eql :key-press)) event)
    (process-key-event window :press event))

  (defmethod process-event ((window window) (type (eql :key-release)) event)
    (process-key-event window :release event)))

(flet ((process-mouse-event (window action event)
         ;; TODO: double click
         (case (xlib:button-event-button event)
           (1 (fb:mouse-button-changed window :left action (xlib:button-event-state event)))
           (2 (fb:mouse-button-changed window :middle action (xlib:button-event-state event)))
           (3 (fb:mouse-button-changed window :right action (xlib:button-event-state event)))
           (4 (fb:mouse-scrolled window  0.0 +1.0))
           (5 (fb:mouse-scrolled window  0.0 -1.0))
           (6 (fb:mouse-scrolled window +1.0  0.0))
           (7 (fb:mouse-scrolled window -1.0  0.0))
           (T (fb:mouse-button-changed window (- (xlib:button-event-button event) 4) action (xlib:button-event-state event))))))

  (defmethod process-event ((window window) (type (eql :button-press)) event)
    (process-mouse-event window :press event))

  (defmethod process-event ((window window) (type (eql :button-release)) event)
    (process-mouse-event window :release event)))

(defmethod process-event ((window window) (type (eql :motion-notify)) event)
  (fb:mouse-moved window (xlib:positioned-event-x event) (xlib:positioned-event-y event)))

(defmethod process-event ((window window) (type (eql :configure-notify)) event)
  (let ((size (fb:size window)))
    (when (or (/= (car size) (xlib:configure-event-width event))
              (/= (cdr size) (xlib:configure-event-height event)))
      (update-buffer window (xlib:configure-event-width event) (xlib:configure-event-height event))
      (fb:window-resized window (xlib:configure-event-width event) (xlib:configure-event-height event))
      (fb:window-refreshed window)))
  (let ((location (fb:location window)))
    (when (or (/= (car location) (xlib:configure-event-x event))
              (/= (cdr location) (xlib:configure-event-y event)))
      (fb:window-moved window (xlib:configure-event-x event) (xlib:configure-event-y event)))))

(defmethod process-event ((window window) (type (eql :property-notify)) event)
  (when (= (xlib:property-event-state event) 0)
    (cond ((atomp window (xlib:property-event-atom event) "WM_STATE")
           (let ((state (get-state window)))
             (case state
               ((:iconic :normal)
                (setf state (eq state :iconic))
                (unless (eq (fb:iconified-p window) state)
                  (fb:window-iconified window state))))))
          ((atomp window (xlib:property-event-atom event) "NET_WM_STATE")
           (cffi:with-foreign-objects ((states :pointer))
             (let ((state (dotimes (i (get-property window "NET_WM_STATE" 4 states))
                            (let ((atom (cffi:mem-aref (cffi:mem-ref states :pointer) 'xlib:atom i)))
                              (when (or (atomp window atom "NET_WM_STATE_MAXIMIZED_VERT")
                                        (atomp window atom "NET_WM_STATE_MAXIMIZED_HORZ"))
                                (return T))))))
               (unless (eq (fb:maximized-p window) state)
                 (fb:window-maximized window state))
               (xlib:free (cffi:mem-ref states :pointer))))))))

(defmethod process-event ((window window) (type (eql :map-notify)) event)
  (setf (fb-int:visible-p window) T)
  ;; Make sure we restore the proper window size.
  (destructuring-bind (w . h) (fb:size window)
    (xlib:resize-window (display window) (xid window) w h)))

(defmethod process-event ((window window) (type (eql :unmap-notify)) event)
  (setf (fb-int:mouse-entered-p window) NIL)
  (setf (fb-int:focused-p window) NIL)
  (setf (fb-int:visible-p window) NIL))

(defmethod process-event ((window window) (type (eql :enter-notify)) event)
  (fb:mouse-entered window T))

(defmethod process-event ((window window) (type (eql :leave-notify)) event)
  (fb:mouse-entered window NIL))

(defmethod process-event ((window window) (type (eql :focus-in)) event)
  (fb:window-focused window T))

(defmethod process-event ((window window) (type (eql :focus-out)) event)
  (fb:window-focused window NIL))

(defmethod process-event ((window window) (type (eql :destroy-notify)) event)
  (fb:window-closed window))

(defmethod process-event ((window window) (type (eql :expose)) event)
  (fb:window-refreshed window))

(defmethod process-event ((window window) (type (eql :client-message)) event)
  (cond ((atomp window (xlib:client-message-event-message-type event) "WM_PROTOCOLS")
         (let ((protocol (xlib:net-message-event-protocol event)))
           (cond ((atomp window protocol "WM_DELETE_WINDOW")
                  (fb:window-closed window))
                 ((atomp window protocol "NET_WM_PING")
                  (cffi:with-foreign-objects ((rpl '(:struct xlib:event)))
                    (cffi:foreign-funcall "memcpy" :pointer rpl :pointer event :size (cffi:foreign-type-size '(:struct xlib:event)))
                    (setf (xlib:base-event-window rpl) (xlib:default-root-window (display window)))
                    ;; SubstructureNotifyMask | SubstructureRedirectMask
                    (xlib:send-event (display window) (xid window) NIL 1572864 rpl))))))
        ((atomp window (xlib:client-message-event-message-type event) "XdndEnter"))
        ((atomp window (xlib:client-message-event-message-type event) "XdndPosition"))
        ((atomp window (xlib:client-message-event-message-type event) "XdndDrop")
         ;; TODO: implement drops
         )))

(defmethod process-event ((window window) (type (eql :selection-notify)) event)
  #++
  (cond ((atomp window (xlib:selection-event-selection-property event) "XdndSelection")
         )))

;; TODO: touch events
;; TODO: pen events
