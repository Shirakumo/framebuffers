(in-package #:org.shirakumo.framebuffers.win32)

(pushnew :win32 sb-int:*available-backends*)

(define-condition win32-error (fb:framebuffer-error com:win32-error)
  ())

(defmethod fb-int:init-backend ((backend (eql :win32)))
  (unless (cffi:foreign-library-loaded-p 'win32:user32)
    (cffi:load-foreign-library 'win32:user32)
    (ignore-errors (cffi:load-foreign-library 'win32:shcore)))
  (com:init))

(defmethod fb-int:shutdown-backend ((backend (eql :win32)))
  (com:shutdown))

(defmethod fb-int:open-backend ((backend (eql :win32)) &key))

(defclass window (fb:window)
  ((pointer :initarg :pointer :accessor pointer)
   (close-requested-p :initform NIL :initarg :close-requested-p :accessor fb:close-requested-p :accessor close-requested-p)
   (size :initform (cons 1 1) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform NIL :initarg :title :reader fb:title :accessor title)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (content-scale :initform NIL :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

(defmethod fb:valid-p ((window window))
  (not (null (pointer window))))

(defmethod fb:close ((window window)))

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

(defmethod fb:buffer ((window window)))

(defmethod fb:swap-buffers ((window window)))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
