(in-package #:org.shirakumo.framebuffers.win32)

(pushnew :win32 sb-int:*available-backends*)

(defmacro with-rect ((rect x y w h) &body body)
  `(cffi:with-foreign-objects ((,rect '(:struct win32:rect)))
     (setf (win32:rect-left ,rect) ,x)
     (setf (win32:rect-top ,rect) ,y)
     (setf (win32:rect-right ,rect) (+ (win32:rect-left ,rect) ,w))
     (setf (win32:rect-bottom ,rect) (+ (win32:rect-top ,rect) ,h))
     ,@body))

(define-condition win32-error (fb:framebuffer-error com:win32-error)
  ())

(defun win32-error (&key function-name message)
  (com:win32-error T :function-name function-name :message message :type 'win32-error))

(defmethod fb-int:init-backend ((backend (eql :win32)))
  (unless (cffi:foreign-library-loaded-p 'win32:user32)
    (cffi:load-foreign-library 'win32:user32)
    (ignore-errors (cffi:load-foreign-library 'win32:shcore)))
  (com:init)
  (or (ignore-errors (win32:set-process-dpi-awareness-context :per-monitor-aware-2))
      (ignore-errors (win32:set-process-dpi-awareness-context :per-monitor-aware))
      (ignore-errors (win32:set-process-dpi-awareness :per-monitor-dpi-aware))
      (ignore-errors (win32:set-process-dpi-aware))))

(defmethod fb-int:shutdown-backend ((backend (eql :win32)))
  (com:shutdown))

(defmethod fb-int:open-backend ((backend (eql :win32)) &key (title (fb-int:default-title)) location size (visible-p T))
  (let* ((screen-w (win32:get-system-metrics :cx-screen))
         (screen-h (win32:get-system-metrics :cy-screen))
         (w (or (car size) screen-w))
         (h (or (cdr size) screen-h))
         (x (or (car location) (round (- screen-w w) 2)))
         (y (or (cdr location) (round (- screen-h h) 2)))
         (style (list :maximizebox :sizebox))
         (class (cffi:foreign-alloc '(:struct win32:window-class))))
    (setf (win32:window-class-style class) '(:owndc :vredraw :hredraw))
    (setf (win32:window-class-proc class) (cffi:callback handle-event))
    (setf (win32:window-class-cursor class) (win32:load-cursor 0 :arrow))
    (setf (win32:window-class-class-name class) "framebuffer")
    (win32:register-class class)
    (let ((ptr (win32:create-window 0 "framebuffer" title style x y w h 0 0 0 0)))
      (when (cffi:null-pointer-p ptr)
        (win32-error :function-name 'win32:create-window))
      (make-instance 'window :ptr ptr 
                             :dc (win32:get-dc ptr)
                             :size (cons w h)
                             :location (cons x y)
                             :visible-p visible-p))))

(defclass window (fb:window)
  ((ptr :initarg :ptr :accessor ptr)
   (dc :initarg :dc :accessor dc)
   (class :initarg :class :accessor class)
   (buffer :reader fb:buffer :accessor buffer)
   (close-requested-p :initform NIL :initarg :close-requested-p :accessor fb:close-requested-p :accessor close-requested-p)
   (size :initform (cons 1 1) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform NIL :initarg :title :reader fb:title :accessor title)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

(defmethod initialize-instance :after ((window window) &key)
  (let ((ptr (ptr window)))
    (setf (fb-int:ptr-window ptr) window)
    (win32:set-window ptr :userdata ptr)
    (when visible-p
      (win32:show-window ptr :normal))))

(defmethod fb:valid-p ((window window))
  (not (null (ptr window))))

(defmethod fb:close ((window window))
  (when (dc window)
    (win32:release-dc (dc window))
    (setf (dc window) NIL))
  (when (ptr window)
    (win32:destroy-window (ptr window))
    (setf (ptr window) NIL)))

(defmethod fb:width ((window window))
  (car (size window)))

(defmethod fb:height ((window window))
  (cdr (size window)))

(defmethod (setf fb:size) (size (window window)))

(defmethod (setf fb:location) (location (window window)))

(defmethod (setf fb:title) (title (window window)))

(defmethod (setf fb:visible-p) (state (window window)))

(defmethod (setf fb:maximized-p) (state (window window)))

(defmethod (setf fb:iconified-p) (state (window window)))

(defmethod fb:clipboard-string ((window window)))

(defmethod (setf fb:clipboard-string) (string (window window)))

(defmethod fb:request-attention ((window window)))

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (with-rect (rect x y w h)
    (win32:invalidate-rect (ptr window) rect T)
    (win32:send-message (ptr window) :paint 0 0))
  (when sync
    ;; TODO: sync
    ))

(defmethod fb:process-events ((window window) &key timeout))
