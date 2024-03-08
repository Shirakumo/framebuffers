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

(defvar *init* NIL)

(defmethod fb-int:init-backend ((backend (eql :xlib)))
  (unless *init*
    (unless (cffi:foreign-library-loaded-p 'xlib:x11)
      (cffi:use-foreign-library xlib:x11))
    (xlib:init-threads)
    (setf *init* T)
    (xlib:set-error-handler (cffi:callback error-handler))
    (xlib:set-io-error-handler (cffi:callback io-error-handler))
      ;; Try to open the display once to ensure we have a connection

    (let ((display (xlib:open-display (cffi:null-pointer))))
      (when (cffi:null-pointer-p display)
        (error "Failed to open display."))
      (unwind-protect (check-pixmap-formats display)
        (xlib:close-display display)))))

(defmethod fb-int:shutdown-backend ((backend (eql :xlib)))
  (when *init*
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
                         (string= "String" (cffi:mem-ref (cffi:mem-ref type :pointer) :string)))
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

(defmethod fb-int:open-backend ((backend (eql :xlib)) &key (size (cons NIL NIL)) (location (cons NIL NIL)) (title (fb-int:default-title)) (visible-p T))
  (with-creation (display (xlib:open-display (cffi:null-pointer))) (xlib:close-display display)
    (xlib:set-io-error-exit-handler display (cffi:callback io-error-exit-handler) (cffi:null-pointer))
    (let* ((screen (xlib:default-screen display))
           (visual (xlib:default-visual display screen))
           (depth (xlib:default-depth display screen)))
      ;; Default size and location
      (unless (car size) (setf (car size) (xlib:display-width display screen)))
      (unless (cdr size) (setf (cdr size) (xlib:display-height display screen)))
      (unless (car location) (setf (car location) (truncate (- (xlib:display-width display screen) (car size)) 2)))
      (unless (cdr location) (setf (cdr location) (truncate (- (xlib:display-height display screen) (cdr size)) 2)))
      (cffi:with-foreign-objects ((attrs '(:struct xlib:set-window-attributes))
                                  (protos :pointer 10))
        (setf (xlib:set-window-attributes-border-pixel attrs) (xlib:black-pixel display screen))
        (setf (xlib:set-window-attributes-background-pixel attrs) (xlib:black-pixel display screen))
        (setf (xlib:set-window-attributes-backing-store attrs) 0)
        (with-creation (window (xlib:create-window display (xlib:default-root-window display)
                                                   (car location) (cdr location) (car size) (cdr size)
                                                   0 depth 1 visual '(:back-pixel :border-pixel :backing-store) attrs))
                       (xlib:destroy-window display window)
          (with-creation (image (xlib:create-image display 0 depth 2 0 0 (car size) (cdr size) 32 (* 4 (car size))))
                         (xlib:destroy-image image)
            (xlib:store-name display window title)
            (xlib:select-input display window '(:key-press :key-release :button-press :button-relaese :pointer-motion
                                                :structure-notify :exposure :focus-change :enter-window :leave-window))
            (setf (cffi:mem-aref protos :pointer 0) (xlib:intern-atom display "WM_DELETE_WINDOW" 0))
            (setf (cffi:mem-aref protos :pointer 1) (xlib:intern-atom display "NET_WM_PING" 0))
            (xlib:set-wm-protocols display window protos 2)
            (xlib:clear-window display window)
            (when visible-p
              (xlib:map-raised display window))
            (xlib:flush display)
            
            (make-instance 'window :display display :image image :xid window :screen screen
                                   :size size :location location :title title
                                   :xkb (probe-xkb display) :visible-p visible-p
                                   :content-scale (content-scale display))))))))

(defclass window (fb:window)
  ((display :initarg :display :accessor display)
   (screen :initarg :screen :accessor screen)
   (xid :initarg :xid :accessor xid)
   (image :initarg :image :accessor image)
   (xkb :initarg :xkb :accessor xkb)

   (buffer :initarg :buffer :reader fb:buffer :accessor buffer)
   (size :initform (cons 0 0) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform "" :initarg :title :reader fb:title :accessor title)
   (close-requested-p :initform NIL :initarg :close-requested-p :reader fb:close-requested-p :accessor close-requested-p)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale)
   (atom-table :initform (make-hash-table :test 'equal) :reader atom-table)))

(defmethod initialize-instance :after ((window window) &key)
  (setf (fb-int:ptr-window (display window)) window)
  (destructuring-bind (w . h) (size window)
    (setf (buffer window) (static-vectors:make-static-vector (* 4 w h)))))

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
  (setf (close-requested-p window) T)
  (when (image window)
    (xlib:destroy-image (image window))
    (setf (image window) NIL))
  (when (xid window)
    (xlib:destroy-window (display window) (xid window))
    (setf (xid window) NIL))
  (when (display window)
    (setf (fb-int:ptr-window (display window)) NIL)
    (xlib:flush (display window))
    (xlib:close-display (display window))
    (setf (display window) NIL)))

(defmethod fb:width ((window window))
  (car (fb:size window)))

(defmethod fb:height ((window window))
  (cdr (fb:size window)))

(defmethod (setf fb:size) (size (window window))
  (destructuring-bind (w . h) size
    (xlib:resize-window (display window) (xid window) w h)
    (xlib:flush (display window))
    (setf (car (size window)) w)
    (setf (cdr (size window)) h)
    size))

(defmethod (setf fb:location) (location (window window))
  (destructuring-bind (x . y) location
    (xlib:move-window (display window) (xid window) x y)
    (xlib:flush (display window))
    (setf (car (location window)) x)
    (setf (cdr (location window)) y)
    location))

(defmethod (setf fb:title) (title (window window))
  (xlib:store-name (display window) (xid window) title)
  (xlib:flush (display window))
  (setf (title window) title))

(defmethod (setf fb:visible-p) (state (window window))
  (if state
      (xlib:map-raised (display window) (xid window))
      (xlib:unmap-window (display window) (xid window)))
  (xlib:flush (display window))
  (setf (visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window))
  (cond (state
         (cond ((visible-p window)
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
        ((maximized-p window)
         (send-client-event window "NET_WM_STATE" 0
                            (atom window "NET_WM_STATE_MAXIMIZED_VERT")
                            (atom window "NET_WM_STATE_MAXIMIZED_HORZ")
                            1 0)))
  (xlib:flush (display window))
  (setf (maximized-p window) state))

(defmethod (setf fb:iconified-p) (state (window window))
  (cond (state
         (xlib:iconify-window (display window) (xid window) (screen window))
         (xlib:flush (display window))
         (setf (iconified-p window) T))
        (T
         (setf (fb:visible-p window) T)
         (setf (iconified-p window) NIL))))

(defmethod fb:clipboard-string ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard-string) (string (window window))
  ;; TODO: implement clipboard setting
  )

(defmethod fb:request-attention ((window window))
  (send-client-event window "NET_WM_STATE" 1 (atom window "NET_WM_STATE_DEMANDS_ATTENTION") 0 1 0)
  (xlib:flush (display window)))

(defmethod fb:swap-buffers ((window window))
  (let ((size (size window))
        (display (display window))
        (image (image window)))
    (setf (xlib:image-data image) (static-vectors:static-vector-pointer (buffer window)))
    (xlib:put-image display (xid window) (xlib:default-gc display (screen window))
                    image 0 0 0 0 (car size) (cdr size))
    (xlib:flush display)))

(defmethod fb:process-events ((window window) &key timeout)
  (etypecase timeout
    (null
     (cffi:with-foreign-objects ((event '(:struct xlib:event)))
       (loop while (and (display window) (xlib:pending (display window)))
             do (xlib:next-event (display window) event)
                (process-event window (xlib:base-event-type event) event))))
    ((or real (eql T))
     ;; TODO: implement via XConnectionNumber and poll()
     )))

(flet ((process-key-event (window action event)
         (when (and (eql action :release) (xlib:events-queued (display window) 1))
           (cffi:with-foreign-objects ((next '(:struct xlib:event)))
             (xlib:peek-event (display window) next)
             (when (and (eql :key-press (xlib:base-event-type next))
                        (= (xlib:positioned-event-time next) (xlib:positioned-event-time event))
                        (< (- (xlib:key-event-keycode next) (xlib:key-event-keycode event)) 20))
               (xlib:next-event (display window) event)
               (setf action :repeat))))
         (let ((code (xlib:key-event-keycode event)))
           (fb:key-changed window (translate-keycode code window) code action (xlib:key-event-state event))
           ;; FIXME: string translation
           )))
  (defmethod process-event ((window window) (type (eql :key-press)) event)
    (process-key-event window :press event))

  (defmethod process-event ((window window) (type (eql :key-release)) event)
    (process-key-event window :release event)))

(flet ((process-mouse-event (window action event)
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
  (let ((size (size window)))
    (when (or (/= (car size) (xlib:configure-event-width event))
              (/= (cdr size) (xlib:configure-event-height event)))
      (setf (car size) (xlib:configure-event-width event))
      (setf (cdr size) (xlib:configure-event-height event))
      (let ((new-image (check-create (xlib:create-image (display window) 0 (xlib:default-depth (display window) (screen window))
                                                        2 0 0 (car size) (cdr size) 32 (* 4 (car size)))))
            (new-buffer (static-vectors:make-static-vector (* 4 (car size) (cdr size)))))
        (xlib:destroy-image (image window))
        (static-vectors:free-static-vector (buffer window))
        (setf (image window) new-image)
        (setf (buffer window) new-buffer))
      (fb:window-resized window (car size) (cdr size))))
  (let ((location (location window)))
    (when (or (/= (car location) (xlib:configure-event-x event))
              (/= (cdr location) (xlib:configure-event-y event)))
      (setf (car location) (xlib:configure-event-x event))
      (setf (cdr location) (xlib:configure-event-y event))
      (fb:window-moved window (car location) (cdr location)))))

(defmethod process-event ((window window) (type (eql :property-notify)) event)
  (when (= (xlib:property-event-state event) 0)
    (cond ((atomp window (xlib:property-event-atom event) "WM_STATE")
           (let ((state (get-state window)))
             (case state
               ((:iconic :normal)
                (setf state (eq state :iconic))
                (unless (eq (iconified-p window) state)
                  (setf (iconified-p window) state)
                  (fb:window-iconified window state))))))
          ((atomp window (xlib:property-event-atom event) "NET_WM_STATE")
           (cffi:with-foreign-objects ((states :pointer))
             (let ((state (dotimes (i (get-property window "NET_WM_STATE" 4 states))
                            (let ((atom (cffi:mem-aref (cffi:mem-ref states :pointer) 'xlib:atom i)))
                              (when (or (atomp window atom "NET_WM_STATE_MAXIMIZED_VERT")
                                        (atomp window atom "NET_WM_STATE_MAXIMIZED_HORZ"))
                                (return T))))))
               (unless (eq (maximized-p window) state)
                 (setf (maximized-p window) state)
                 (fb:window-maximized window state))
               (xlib:free (cffi:mem-ref states :pointer))))))))

(defmethod process-event ((window window) (type (eql :map-notify)) event)
  (setf (visible-p window) T)
  ;; Make sure we restore the proper window size.
  (destructuring-bind (w . h) (size window)
    (xlib:resize-window (display window) (xid window) w h)))

(defmethod process-event ((window window) (type (eql :unmap-notify)) event)
  (setf (visible-p window) NIL))

(defmethod process-event ((window window) (type (eql :enter-notify)) event)
  (fb:mouse-entered window T))

(defmethod process-event ((window window) (type (eql :leave-notify)) event)
  (fb:mouse-entered window NIL))

(defmethod process-event ((window window) (type (eql :focus-in)) event)
  (fb:window-focused window T))

(defmethod process-event ((window window) (type (eql :focus-out)) event)
  (fb:window-focused window NIL))

(defmethod process-event ((window window) (type (eql :destroy-notify)) event)
  (setf (close-requested-p window) T))

(defmethod process-event ((window window) (type (eql :expose)) event)
  (fb:window-refreshed window))

(defmethod process-event ((window window) (type (eql :client-message)) event)
  (cond ((atomp window (xlib:client-message-event-message-type event) "WM_PROTOCOLS")
         (let ((protocol (xlib:net-message-event-protocol event)))
           (cond ((atomp window protocol "WM_DELETE_WINDOW")
                  (setf (close-requested-p window) T))
                 ((atomp window protocol "NET_WM_PING")
                  (cffi:with-foreign-objects ((rpl '(:struct xlib:event)))
                    (cffi:foreign-funcall "memcpy" :pointer rpl :pointer event :size (cffi:foreign-type-size '(:struct xlib:event)))
                    (setf (xlib:base-event-window rpl) (xlib:default-root-window (display window)))
                    ;; SubstructureNotifyMask | SubstructureRedirectMask
                    (xlib:send-event (display window) (xid window) NIL 1572864 rpl))))))
        ((atom window (xlib:client-message-event-message-type event) "XdndDrop")
         ;; TODO: implement drops
         )))
