(in-package #:org.shirakumo.framebuffers.wayland)

(pushnew :wayland sb-int:*available-backends*)

(define-condition wayland-error (fb:framebuffer-error)
  ((code :initarg :code :initform NIL :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "A Wayland call failed~@[ (~a)~]~@[:~%  ~a~]"
                                 (code c) (message c)))))

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
   (listener :initform (make-listener) :accessor listener)
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
    (wl:proxy-add-listener (registry window) (registry-listener (listener window)) display)))

(defmethod fb:valid-p ((window window))
  (not (null (display window))))

(defmethod fb:close ((window window))
  (dolist (slot '(shell-surface shell surface shm-pool shm compositor keyboard registry))
    (when (slot-value window slot)
      (fb:proxy-destroy (slot-value window slot))
      (setf (slot-value window slot) NIL)))
  (when (listener window)
    (cffi:foreign-free (listener window))
    (setf (listener window) NIL))
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

(defmethod fb:request-attention ((window window)))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd :int)
  (events :short)
  (revents :short))

(defmethod fb:process-events ((window window) &key timeout)
  (let ((display (display window)))
    (cffi:with-foreign-objects ((fd '(:struct pollfd)))
      (setf (pollfd-fd fd) (wl:display-get-fd display))
      (setf (pollfd-events fd) 1)
      (setf (pollfd-revents fd) 0)
      (flet ((poll (millis)
               (let ((res (cffi:foreign-funcall "poll" :pointer fd :int 1 :int millis :int)))
                 (cond ((< 0 res)
                        (wl:display-read-events display)
                        (wl:display-dispatch-pending display))
                       ((< res 0)
                        (wl:display-cancel-read display)
                        NIL)))))
        (etypecase timeout
          (null
           (wl:display-dispatch-pending display))
          (real
           (cond ((/= 0 (wl:display-prepare-read display))
                  (wl:display-dispatch-pending display))
                 (T
                  (wl:display-flush display)
                  (poll (truncate (* 1000 timeout))))))
          ((eql T)
           (loop while (display window)
                 do (loop while (/= 0 (wl:display-prepare-read display))
                          do (wl:display-dispatch-pending display))
                    (poll 1000))))))))

(defmacro define-listener (name &body callbacks)
  `(wl:define-listener ,name
     ,@(loop for (cb args . body) in callbacks
             collect (if body
                         `(,cb :void ((window :pointer) ,@args)
                               (declare (ignorable ,@(mapcar #'car args)))
                               (let ((window (fb-int:ptr-window window)))
                                 (when window
                                   ,@body)))
                         `(,cb NIL)))))

(trivial-indent:define-indentation define-listener
    (4 &rest (&whole 2 6 &body)))

(defmacro define-whole-listener (&rest listeners)
  `(progn 
     (cffi:defcstruct listener
       ,@(loop for listener in listeners
               collect `(,listener (:struct ,listener))))

     ,@(loop for listener in listeners
             collect `(defun ,listener (struct)
                        (cffi:foreign-slot-pointer struct '(:struct listener) ',listener)))

     (defun make-listener (&optional (struct (cffi:foreign-alloc '(:struct listener))))
       ,@(loop for listener in listeners
               collect `(,(intern (format NIL "~a-~a" (symbol-name 'make) (symbol-name listener))) (,listener struct)))
       struct)))

(define-whole-listener
  display-listener
  registry-listener
  shm-listener
  seat-listener
  pointer-listener
  keyboard-listener
  shell-surface-listener
  frame-listener)

(define-listener display-listener
  (error ((display :pointer) (object-id :pointer) (code :uint32) (message :string))
    (error 'wayland-error :window window :code code :message message))
  
  (delete-id))

(define-listener registry-listener
  (global ((registry :pointer) (id :uint32) (interface :string) (version :uint32))
    (cond ((string= interface "wl_compositor")
           (setf (compositor window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:compositor-interface) 1)))
          ((string= interface "wl_shm")
           (setf (shm window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shm-interface) 1))
           (wl:proxy-add-listener (shm window) (shm-listener (listener window)) (display window)))
          ((string= interface "wl_shell")
           (setf (shell window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shell-interface) 1)))
          ((string= interface "wl_seat")
           (setf (seat window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:seat-interface) 1))
           (wl:proxy-add-listener (seat window) (seat-listener (listener window)) (display window)))))

  (global-remove))

(define-listener shm-listener
  (format ((shm :pointer) (format :uint32))
    ))

(define-listener seat-listener
  (capabilities ((seat :pointer) (caps wl:seat-capabilities))
    )

  (name ((seat :pointer) (name :string))))

(define-listener pointer-listener
  (enter ((pointer :pointer) (serial :uint32) (surface :pointer) (sx :uint32) (sy :uint32)))
  
  (leave ((pointer :pointer) (serial :uint32) (surface :pointer)))
  
  (motion ((pointer :pointer) (time :uint32) (sx :uint32) (sy :uint32)))
  
  (button ((pointer :pointer) (serial :uint32) (time :uint32) (button :uint32) (state :uint32)))
  
  (axis ((pointer :pointer) (time :uint32) (axis :uint32) (value :uint32)))
  
  (frame ((pointer :pointer)))
  
  (axis-source ((pointer :pointer) (axis-source :uint32)))
  
  (axis-stop ((pointer :pointer) (time :uint32) (axis :uint32)))
  
  (axis-discrete ((pointer :pointer) (axis :uint32) (discrete :int32))))

(define-listener keyboard-listener
  (keymap ((keyboard :pointer) (format :uint32) (fd :int) (size :uint32)))
  
  (enter ((keyboard :pointer) (serial :uint32) (surface :pointer) (keys :pointer)))
  
  (leave ((keyboard :pointer) (serial :uint32) (surface :pointer)))
  
  (key ((keyboard :pointer) (serial :uint32) (time :uint32) (key :uint32) (state :uint32)))
  
  (modifiers ((keyboard :pointer) (serial :uint32) (mods-depressed :uint32) (mods-latched :uint32) (mods-locked :uint32) (group :uint32)))
  
  (repeat-info ((keyboard :pointer) (rate :int32) (delay :int32))))

(define-listener shell-surface-listener
  (ping ((shell-surface :pointer) (serial :uint32))
    (wl:shell-surface-pong shell-surface serial))

  (configure ((shell-surface :pointer) (edges :uint32) (width :int32) (height :int32)))

  (popup-done ((shell-surface :pointer))))

(define-listener frame-listener
  (done ((callback :pointer) (cookie :uint32))
    (wl:proxy-destroy callback)
    ))
