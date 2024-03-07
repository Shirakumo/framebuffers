(in-package #:org.shirakumo.framebuffers.xlib)
(pushnew :xlib fb-int:*available-backends*)

(defun check-pixmap-formats (display)
  (let* ((screen (xlib:default-screen display))
         (visual (xlib:default-visual display screen))
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

(defmethod fb-int:init-backend ((backend (eql :xlib)))
  (unless (cffi:foreign-library-loaded-p 'xlib:x11)
    (cffi:use-foreign-library xlib:x11)
    (xlib:init-threads)
    ;; Try to open the display once to ensure we have a connection
    (let ((display (xlib:open-display (cffi:null-pointer))))
      (when (cffi:null-pointer-p display)
        (error "Failed to open display."))
      (unwind-protect (check-pixmap-formats display)
        (xlib:close-display display)))))

(defmethod fb-int:shutdown-backend ((backend (eql :xlib)))
  (when (cffi:foreign-library-loaded-p 'xlib:x11)
    (xlib:free-threads)))

(defun check-create (result)
  (if (etypecase result
        (cffi:foreign-pointer (cffi:null-pointer-p result))
        (integer (= 0 result)))
      (error "Failed to create X11 object.")
      result))

(defmacro with-creation ((var creator) cleanup &body body)
  (let ((ok (gensym "OK")))
    `(let ((,ok NIL)
           (,var ,creator))
       (when (etypecase ,var
               (cffi:foreign-pointer (cffi:null-pointer-p ,var))
               (integer (= 0 ,var)))
         (error "Failed to ~a" ',(car creator)))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (setf ,ok T))
         (unless ,ok
           ,cleanup)))))

(defmethod fb-int:open-backend ((backend (eql :xlib)) &key (size (cons NIL NIL)) (location (cons NIL NIL)) (title (fb-int:default-title)) (visible-p T))
  (with-creation (display (xlib:open-display (cffi:null-pointer))) (xlib:close-display display)
    (let* ((screen (xlib:default-screen display))
           (visual (xlib:default-visual display screen))
           (depth (xlib:default-depth display screen)))
      ;; Default size and location
      (unless (car size) (setf (car size) (xlib:display-width display screen)))
      (unless (cdr size) (setf (cdr size) (xlib:display-height display screen)))
      (unless (car location) (setf (car location) (truncate (- (xlib:display-width display scree3n) (car size)) 2)))
      (unless (cdr location) (setf (cdr location) (truncate (- (xlib:display-height display screen) (cdr size)) 2)))
      (cffi:with-foreign-objects ((attrs '(:struct xlib:set-window-attributes)))
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
            ;; TODO: stuff like borderless, always on top, fullscreen, resizable, min/max size, etc.
            (xlib:set-wm-protocols display window (xlib:intern-atom display "WM_DELETE_WINDOW" 0) 1)
            (xlib:clear-window display window)
            (when visible-p
              (xlib:map-raised display window))
            (xlib:flush display)
            (make-instance 'window :display display :image image :xid window
                                   :size size :location location :title title
                                   :visible-p visible-p)))))))

(defclass window (fb:window)
  ((display :initarg :display :accessor display)
   (xid :initarg :xid :accessor xid)
   (image :initarg :image :accessor image)

   (size :initform (cons 0 0) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform "" :initarg :title :reader fb:title :accessor title)
   (close-requested-p :initform NIL :initarg :close-requested-p :reader fb:close-requested-p :accessor close-requested-p)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

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
    (xlib:close-display (display window))
    (setf (display window) NIL)))

(defmethod fb:width ((window window))
  (car (fb:size window)))

(defmethod fb:height ((window window))
  (cdr (fb:size window)))

(defmethod (setf fb:size) (size (window window))
  (destructuring-bind (w . h) size
    (xlib:resize-window (display window) (xid window) w h)
    (setf (car (size window)) w)
    (setf (cdr (size window)) h)
    size))

(defmethod (setf fb:location) (location (window window))
  (destructuring-bind (x . y) location
    (xlib:move-window (display window) (xid window) x y)
    (setf (car (location window)) x)
    (setf (cdr (location window)) y)
    location))

(defmethod (setf fb:title) (title (window window))
  (xlib:store-name display window title)
  (setf (title window) title))

(defmethod (setf fb:visible-p) (state (window window))
  (if state
      (xlib:map-raised (display window) (xid window))
      (xlib:unmap-window (display window) (xid window)))
  (setf (visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window)))

(defmethod (setf fb:iconified-p) (state (window window)))

(defmethod fb:clipboard-string ((window window)))

(defmethod (setf fb:clipboard-string) (string (window window)))

(defmethod fb:swap-buffers ((window window) new-buffer))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
