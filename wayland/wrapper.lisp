(in-package #:org.shirakumo.framebuffers.wayland)

(pushnew :wayland sb-int:*available-backends*)

(define-condition wayland-error (fb:framebuffer-error)
  ((message :initarg :message :initform NIl :reader message))
  (:report (lambda (c s) (format s ""))))

(defmethod fb-int:init-backend ((backend (eql :wayland)))
  (unless (cffi:foreign-library-loaded-p 'wl:wayland)
    (cffi:use-foreign-library wl:wayland)
    (let ((display (wl:display-connect (cffi:null-pointer))))
      (if (cffi:null-pointer-p display)
          (error "Failed to connect to Wayland display.")
          (wl:display-disconnect display)))))

(defmethod fb-int:shutdown-backend ((backend (eql :wayland))))

(defmethod fb-int:open-backend ((backend (eql :wayland)) &key)
  (let* ((display (wl:display-connect (cffi:null-pointer)))
         (window (make-instance 'window :display display)))
    (when (= -1 (wl:display-dispatch display))
      (error 'wayland-error :message "Failed to dispatch display."))
    (when (= -1 (wl:display-roundtrip display))
      (error 'wayland-error :message "Failed to roundtrip display."))
    (when (= -1 (shm-format window))
      (error 'wayland-error :message "No suitable pixel format."))
    (when (= -1 (compositor window))
      (error 'wayland-error :message "Couldn't find a compositor."))))

(defclass window (fb:window)
  ((display :initarg :display :initform NIL :accessor display)
   (shell-surface :initform NIL :accessor shell-surface)
   (shell :initform NIL :accessor shell)
   (surface :initform NIL :accessor surface)
   (draw-buffer :initform NIL :accessor draw-buffer)
   (shm-pool :initform NIL :accessor shm-pool)
   (shm :initform NIL :accessor shm)
   (compositor :initform NIL :accessor compositor)
   (keyboard :initform NIL :accessor keyboard)
   (seat :initform NIL :accessor seat)
   (registry :initform NIL :accessor registry)

   (buffer :initform NIL :reader fb:buffer :accessor buffer)
   (content-scale :initform (cons 1 1) :reader fb:content-scale :accessor content-scale)
   (close-requested-p :initform NIL :reader fb:close-requested-p :accessor close-requested-p)
   (size :initform (cons 1 1) :reader fb:size :accessor size)
   (location :initform (cons 0 0) :reader fb:location :accessor location)
   (title :initform NIL :reader fb:title :accessor title)
   (visible-p :initform NIL :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :reader fb:iconified-p :accessor iconified-p)))

(defmethod initialize-instance :after ((window window) &key)
  (let ((display (display window)))
    (setf (fb-int:ptr-window display) window)
    (setf (registry window) (wl:display-get-registry display))
    (wl:proxy-add-listener (registry window) listener display)
    ))

(defmethod fb:valid-p ((window window))
  (not (null (display window))))

(defmethod fb:close ((window window))
  (dolist (slot '(shell-surface shell surface shm-pool shm compositor keyboard registry))
    (when (slot-value window slot)
      (fb:proxy-destroy (slot-value window slot))
      (setf (slot-value window slot) NIL)))
  (when (draw-buffer window)
    (wl:buffer-destroy (draw-buffer window))
    (setf (draw-buffer window) NIL))
  (when (buffer window)
    (static-vectors:free-static-vector (buffer window))
    (setf (buffer window) NIL))
  (when (display window)
    (wl:display-disconnect (display window))
    (setf (display window) NIL)))

(defmethod fb:width ((window window))
  (car (fb:size window)))

(defmethod fb:height ((window window))
  (cdr (fb:size window)))

(defmethod (setf fb:size) (size (window window)))

(defmethod (setf fb:location) (location (window window)))

(defmethod (setf fb:title) (title (window window))
  (wl:shell-surface-set-title (shell-surface window) title)
  (setf (title window) title))

(defmethod (setf fb:visible-p) (state (window window)))

(defmethod (setf fb:maximized-p) (state (window window)))

(defmethod (setf fb:iconified-p) (state (window window)))

(defmethod fb:clipboard-string ((window window)))

(defmethod (setf fb:clipboard-string) (string (window window)))

(defmethod fb:swap-buffers ((window window)))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
