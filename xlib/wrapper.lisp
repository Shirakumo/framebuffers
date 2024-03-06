(in-package #:org.shirakumo.framebuffers.xlib)
(pushnew :xlib fb-int:*available-backends*)

(defmethod fb-int:init-backend ((backend (eql :xlib)))
  (unless (cffi:foreign-library-loaded-p 'xlib:x11)
    (cffi:use-foreign-library xlib:x11)
    (xlib:init-threads)))

(defmethod fb-int:shutdown-backend ((backend (eql :xlib)))
  (when (cffi:foreign-library-loaded-p 'xlib:x11)
    (xlib:free-threads)))

(defmethod fb-int:open-backend ((backend (eql :xlib)) &key width height (title (fb-int:default-title)) visible-p)
  (let ((display (xlib:open-display (cffi:null-pointer))))
    (when (cffi:null-pointer-p display)
      (error "Failed to open display."))
    ))

(defclass window (fb:window)
  ((display :initarg :display :accessor display)
   (xid :initarg :xid :accessor xid)

   (size :initform (cons 0 0) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform "" :initarg :title :reader fb:title :accessor title)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (content-scale :initform (cons 1f0 1f0) :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

(defmethod fb:close ((window window))
  (when (display window)
    (setf (xid window) NIL)
    (xlib:close-display (display window))
    (setf (display window) NIL)))

(defmethod fb:width ((window window))
  (car (fb:size window)))

(defmethod fb:height ((window window))
  (cdr (fb:size window)))

(defmethod (setf fb:size) (size (window window)))

(defmethod (setf fb:location) (location (window window)))

(defmethod (setf fb:title) (title (window window)))

(defmethod (setf fb:visible-p) (state (window window)))

(defmethod (setf fb:maximized-p) (state (window window)))

(defmethod (setf fb:iconified-p) (state (window window)))

(defmethod fb:clipboard-string ((window window)))

(defmethod (setf fb:clipboard-string) (string (window window)))

(defmethod fb:swap-buffers ((window window) new-buffer))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
