(in-package #:org.shirakumo.framebuffers.int)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname* (error "Need compile-file or load."))))
(defvar *windows-table* (make-hash-table :test 'eql))
(defvar *available-backends* ())
(defvar *backend* NIL)

(defgeneric init-backend (backend))
(defgeneric shutdown-backend (backend))
(defgeneric open-backend (backend &key))
(defgeneric list-displays-backend (backend))

(defun static-file (path)
  (merge-pathnames path *here*))

(defun default-title ()
  (format NIL "Framebuffer (~a ~a)" (lisp-implementation-type) (lisp-implementation-version)))

(declaim (inline ptr-int))
(defun ptr-int (ptr)
  (etypecase ptr
    #+cffi
    (cffi:foreign-pointer (cffi:pointer-address ptr))
    ((integer 1) ptr)))

(declaim (inline ptr-window))
(defun ptr-window (ptr)
  (gethash (ptr-int ptr) *windows-table*))

(defun (setf ptr-window) (window ptr)
  (if window
      (setf (gethash (ptr-int ptr) *windows-table*) window)
      (remhash (ptr-int ptr) *windows-table*))
  window)

(defun list-windows ()
  (loop for window being the hash-values of *windows-table*
        collect window))

(defun init (&optional backend)
  (cond (backend
         (init-backend backend)
         backend)
        (T
         (dolist (backend *available-backends*)
           (handler-case
               (progn (init-backend backend)
                      (setf *backend* backend)
                      (return-from init backend))
             (error ())))
         (if *available-backends*
             (error "Tried to configure ~{~a~^, ~a~}, but none would start properly." *available-backends*)
             (error "There are no available backends for your system.")))))

(defun shutdown ()
  (when *backend*
    (dolist (window (list-windows))
      (ignore-errors (close window)))
    (shutdown-backend (shiftf *backend* NIL))
    (clrhash *windows-table*)))

(defun list-displays ()
  (list-displays-backend (or *backend* (init))))

(defun open (&rest args &key size location title visible-p maximum-size minimum-size maximized-p iconified-p borderless-p always-on-top-p floating-p &allow-other-keys)
  (declare (ignore size location title visible-p))
  (let ((window (apply #'open-backend (or *backend* (init))
                       ;; We filter this way to allow backend-specific extension args
                       (loop for (k v) on args by #'cddr
                             for culled-prop-p = (find k '(:maximum-size :minimum-size :maximized-p :iconified-p :borderless-p :always-on-top-p :floating-p))
                             unless culled-prop-p collect k
                             unless culled-prop-p collect v))))
    (when maximum-size (setf (maximum-size window) maximum-size))
    (when minimum-size (setf (minimum-size window) minimum-size))
    (when maximized-p (setf (maximized-p window) maximized-p))
    (when iconified-p (setf (iconified-p window) iconified-p))
    (when borderless-p (setf (borderless-p window) borderless-p))
    (when always-on-top-p (setf (always-on-top-p window) always-on-top-p))
    (when floating-p (setf (floating-p window) floating-p))
    window))

(defmethod print-object ((icon fb:icon) stream)
  (print-unreadable-object (icon stream :type T :identity T)
    (format stream "~a x ~a" (width icon) (height icon))))

(defmethod width ((icon fb:icon))
  (icon-width icon))

(defmethod (setf width) (value (icon fb:icon))
  (setf (icon-width icon) value))

(defmethod height ((icon fb:icon))
  (icon-height icon))

(defmethod (setf height) (value (icon fb:icon))
  (setf (icon-height icon) value))

(defmethod buffer ((icon fb:icon))
  (icon-buffer icon))

(defmethod (setf buffer) (value (icon fb:icon))
  (setf (icon-buffer icon) value))

(defmethod print-object ((touchpoint touchpoint) stream)
  (print-unreadable-object (touchpoint stream :type T :identity T)
    (format stream "~a" (location touchpoint))))

(defmethod fb:location ((touchpoint touchpoint))
  (touchpoint-location touchpoint))

(defmethod location ((touchpoint touchpoint))
  (touchpoint-location touchpoint))

(defmethod (setf location) (location (touchpoint touchpoint))
  (setf (touchpoint-location touchpoint) location))

(defmethod fb:radius ((touchpoint touchpoint))
  (touchpoint-radius touchpoint))

(defmethod radius ((touchpoint touchpoint))
  (touchpoint-radius touchpoint))

(defmethod (setf radius) (radius (touchpoint touchpoint))
  (setf (touchpoint-radius touchpoint) radius))

(defmethod fb:angle ((touchpoint touchpoint))
  (touchpoint-angle touchpoint))

(defmethod angle ((touchpoint touchpoint))
  (touchpoint-angle touchpoint))

(defmethod (setf angle) (angle (touchpoint touchpoint))
  (setf (touchpoint-angle touchpoint) angle))

(defmethod fb:pressure ((touchpoint touchpoint))
  (touchpoint-pressure touchpoint))

(defmethod pressure ((touchpoint touchpoint))
  (touchpoint-pressure touchpoint))

(defmethod (setf pressure) (pressure (touchpoint touchpoint))
  (setf (touchpoint-pressure touchpoint) pressure))

(defmethod print-object ((video-mode fb:video-mode) stream)
  (print-unreadable-object (video-mode stream :type T)
    (format stream "~a" (fb:title video-mode))))

(defmethod fb:display ((video-mode fb:video-mode))
  (video-mode-display video-mode))

(defmethod display ((video-mode fb:video-mode))
  (video-mode-display video-mode))

(defmethod (setf display) (display (video-mode fb:video-mode))
  (setf (video-mode-display video-mode) display))

(defmethod width ((video-mode fb:video-mode))
  (video-mode-width video-mode))

(defmethod height ((video-mode fb:video-mode))
  (video-mode-height video-mode))
fb:
(defmethod refresh-rate ((video-mode fb:video-mode))
  (video-mode-refresh-rate video-mode))

(defmethod fb:id ((video-mode fb:video-mode))
  (format NIL "~dx~d@~d-~a"
          (width video-mode) (height video-mode)
          (refresh-rate video-mode) (id (display video-mode))))

(defmethod fb:title ((video-mode fb:video-mode))
  (format NIL "~dx~d @ ~dHz ~a"
          (width video-mode) (height video-mode)
          (refresh-rate video-mode) (or (title (display video-mode)) (id (display video-mode)))))

(defclass display ()
  ((id :initarg :id :reader fb:id :accessor id)
   (title :initarg :title :initform NIL :reader fb:title :accessor title)
   (size :initform (cons 0 0) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (primary-p :initform NIL :initarg :primary-p :reader fb:primary-p :accessor primary-p)
   (video-mode :initform NIL :initarg :video-mode :reader fb:video-mode :accessor video-mode)
   (video-modes :initform () :initarg :video-modes :reader fb:video-modes :accessor video-modes)))

(defmethod print-object ((display display) stream)
  (print-unreadable-object (display stream :type T)
    (format stream "~a~@[ PRIMARY~]" (or (title display) (id display)) (primary-p display))))

(defmethod width ((display display))
  (car (size display)))

(defmethod height ((display display))
  (cdr (size display)))

(defmethod (setf video-mode) :after ((video-mode fb:video-mode) (display display))
  (setf (car (size display)) (fb:width video-mode))
  (setf (cdr (size display)) (fb:height video-mode)))

(defclass window ()
  ((event-handler :initform (make-instance 'event-handler) :accessor event-handler)
   (mouse-location :initform (cons 0 0) :accessor mouse-location)
   (key-states :initform (make-array 356 :element-type 'bit) :accessor key-states)
   (mouse-states :initform (make-array 10 :element-type 'bit) :accessor mouse-states)
   (close-requested-p :initform NIL :initarg :close-requested-p :accessor fb:close-requested-p :accessor close-requested-p)
   (maximum-size :initform (cons NIL NIL) :initarg :maximum-size :reader fb:maximum-size :accessor maximum-size)
   (minimum-size :initform (cons 1 1) :initarg :minimum-size :reader fb:minimum-size :accessor minimum-size)
   (size :initform (cons 0 0) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform "" :initarg :title :reader fb:title :accessor title)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (focused-p :initform NIL :initarg :focused-p :reader fb:focused-p :accessor focused-p)
   (borderless-p :initform NIL :initarg :borderless-p :reader fb:borderless-p :accessor borderless-p)
   (always-on-top-p :initform NIL :initarg :always-on-top-p :reader fb:always-on-top-p :accessor always-on-top-p)
   (resizable-p :initform NIL :initarg :resizable-p :reader fb:resizable-p :accessor resizable-p)
   (floating-p :initform NIL :initarg :floating-p :reader fb:floating-p :accessor floating-p)
   (mouse-entered-p :initform NIL :initarg :mouse-entered-p :reader fb:mouse-entered-p :accessor mouse-entered-p)
   (fullscren-p :initform NIL :initarg :fullscreen-p :reader fb:fullscreen-p :accessor fullscreen-p)
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale :accessor content-scale)
   (icon :initform NIL :initarg :icon :reader fb:icon :accessor icon)
   (cursor-icon :initform :arrow :initarg :cursor-icon :reader fb:cursor-icon :accessor cursor-icon)
   (cursor-state :initform :normal :initarg :cursor-state :reader fb:cursor-state :accessor cursor-state)))

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type T :identity T)
    (if (valid-p window)
        (format stream "~dx~d" (width window) (height window))
        (format stream "CLOSED"))))

(defmethod initialize-instance :after ((window window) &key event-handler)
  (setf (event-handler window) event-handler))

(defmethod (setf event-handler) :before ((handler event-handler) (window window))
  (setf (window handler) window))

(defmethod mouse-button-pressed-p (button (window window))
  (< 0 (sbit (mouse-states window) (case button
                                     (:left 0)
                                     (:right 1)
                                     (:middle 2)
                                     (T (+ 3 button))))))

(defmethod key-pressed-p ((scancode integer) (window window))
  (when (<= 0 scancode 355)
    (< 0 (sbit (key-states window) scancode))))

(defmethod key-pressed-p ((key symbol) (window window))
  (let ((scancode (key-scan-code key window)))
    (when (<= 0 scancode 355)
      (< 0 (sbit (key-states window) scancode)))))

(defmethod local-key-string ((key symbol) (window window))
  (let ((code (key-scan-code key window)))
    (when code (local-key-string code window))))

(defmethod width ((window window))
  (car (size window)))

(defmethod height ((window window))
  (cdr (size window)))

(defun find-mode-by-id (id)
  (loop for display in (list-displays)
        do (when (string= id (fb:id display))
             (return-from find-mode-by-id display))
           (loop for mode in (video-modes display)
                 do (when (string= id (fb:id mode))
                      (return-from find-mode-by-id mode)))))

(defmethod (setf fb:fullscreen-p) ((string string) (window window))
  (setf (fb:fullscreen-p window) (or (find-mode-by-id string) T)))

(defmethod (setf fb:fullscreen-p) ((default (eql T)) (window window))
  (setf (fb:fullscreen-p window) (or (fb:display window)
                                     (find-if #'fb:primary-p (list-displays))
                                     (first (list-displays)))))

(defmethod (setf fb:fullscreen-p) ((display display) (window window))
  (setf (fb:fullscreen-p window) (video-mode display)))

(defmethod (setf fb:cursor-icon) ((value null) (window window))
  (setf (fb:cursor-icon window) :arrow))

(defmacro define-event-callback (name args)
  `(progn
     (defmethod ,name ((window window) ,@args)
       (,name (event-handler window) ,@args))
     
     (defmethod ,name ((handler event-handler) ,@args))

     (defmethod ,name ((handler dynamic-event-handler) ,@args)
       (funcall (handler handler) ',name (window handler) ,@args))))

(defclass dynamic-event-handler (event-handler)
  ((handler :initarg :handler :accessor handler)))

(define-event-callback window-moved (xpos ypos))
(define-event-callback window-resized (width height))
(define-event-callback window-refreshed ())
(define-event-callback window-focused (focused-p))
(define-event-callback window-iconified (iconified-p))
(define-event-callback window-maximized (maximized-p))
(define-event-callback window-closed ())
(define-event-callback mouse-button-changed (button action modifiers))
(define-event-callback mouse-moved (xpos ypos))
(define-event-callback mouse-entered (entered-p))
(define-event-callback mouse-scrolled (xoffset yoffset))
(define-event-callback key-changed (key scan-code action modifiers))
(define-event-callback string-entered (string))
(define-event-callback file-dropped (paths))
(define-event-callback content-scale-changed (xscale yscale))
(define-event-callback touch-started (points))
(define-event-callback touch-moved (points))
(define-event-callback touch-ended (points))
(define-event-callback touch-cancelled (points))
(define-event-callback pen-moved (xpos ypos mode pressure xtilt ytilt))
(define-event-callback timer-triggered (timer))

(defmethod window-moved :before ((window window) x y)
  (setf (car (location window)) x)
  (setf (cdr (location window)) y))

(defmethod window-resized :before ((window window) width height)
  (setf (car (size window)) width)
  (setf (cdr (size window)) height))

(defmethod window-focused :before ((window window) focused-p)
  (unless focused-p
    (fill (mouse-states window) 0)
    (fill (key-states window) 0))
  (setf (focused-p window) focused-p))

(defmethod window-iconified :before ((window window) iconified-p)
  (when iconified-p
    (fill (mouse-states window) 0)
    (fill (key-states window) 0))
  (setf (iconified-p window) iconified-p))

(defmethod window-maximized :before ((window window) maximized-p)
  (setf (maximized-p window) maximized-p))

(defmethod window-closed :before ((window window))
  (setf (close-requested-p window) T))

(defmethod mouse-button-changed :before ((window window) button action modifiers)
  (let ((scan-code (case button
                     (:left 0)
                     (:right 1)
                     (:middle 2)
                     (T (+ 3 button)))))
    (case action
      (:press (setf (sbit (mouse-states window) scan-code) 1))
      (:release (setf (sbit (mouse-states window) scan-code) 0)))))

(defmethod mouse-moved :before ((window window) xpos ypos)
  (setf (car (mouse-location window)) xpos)
  (setf (car (mouse-location window)) ypos))

(defmethod mouse-entered :before ((window window) entered-p)
  (setf (mouse-entered-p window) entered-p))

(defmethod key-changed :before ((window window) key scan-code action modifiers)
  (when (<= 0 scan-code 355)
    (case action
      (:press (setf (sbit (key-states window) scan-code) 1))
      (:release (setf (sbit (key-states window) scan-code) 0)))))

(defmethod content-scale-changed :before ((window window) xscale yscale)
  (setf (car (content-scale window)) xscale)
  (setf (car (content-scale window)) yscale))

(defun resize-buffer (w h &optional old-buffer ow oh)
  (let ((buffer (static-vectors:make-static-vector (* 4 w h) :initial-element 0)))
    (when old-buffer
      ;; Copy sub-region back.
      ;; TODO: scale it instead
      (dotimes (y (min h oh))
        (dotimes (x (min w ow))
          (dotimes (z 4)
            (setf (aref buffer (+ z (* 4 (+ x (* w y)))))
                  (aref old-buffer (+ z (* 4 (+ x (* ow y)))))))))
      (static-vectors:free-static-vector old-buffer))
    buffer))

(defmacro with-cleanup (cleanup &body body)
  (let ((ok (gensym "OK")))
    `(let ((,ok NIL))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (setf ,ok T))
         (unless ,ok
           ,cleanup)))))

(defmacro clean (window accessor free)
  `(let ((,accessor (,accessor ,window)))
     (when ,accessor
       (,free ,accessor)
       (setf (,accessor ,window) NIL))))

(defmacro with-window ((window &rest initargs) &body handlers)
  (let ((handle (gensym "HANDLE"))
        (event-type (gensym "EVENT-TYPE"))
        (args (gensym "ARGS")))
    `(flet ((,handle (,event-type ,window &rest ,args)
              (case ,event-type
                ,@(loop for (type lambda-list . body) in handlers
                        collect (if (eql T type)
                                    `(,type (destructuring-bind ,lambda-list (list* ,event-type ,args)
                                              ,@body))
                                    `(,type (destructuring-bind ,lambda-list ,args
                                              ,@body)))))))
       (let ((,window (open :event-handler (make-instance 'dynamic-event-handler :handler #',handle) ,@initargs)))
         (unwind-protect
              (loop initially (,handle 'init ,window)
                    finally (,handle 'shutdown ,window)
                    until (close-requested-p ,window)
                    do (process-events ,window :timeout T))
           (close ,window))))))

(trivial-indent:define-indentation with-window
    (4 &rest (&whole 2 6 &body)))

(defmacro do-pixels ((buf i x y) window &body body)
  (let ((wg (gensym "WIDTH"))
        (hg (gensym "HEIGHT"))
        (win (gensym "WINDOW")))
    `(let* ((,i 0)
            (,win ,window)
            (,buf (buffer ,win)))
       (destructuring-bind (,wg . ,hg) (size ,win)
         (declare (type (simple-array (unsigned-byte 8) (*)) ,buf))
         (declare (type (unsigned-byte 32) ,i ,wg ,hg))
         (dotimes (,y ,hg ,buf)
           (dotimes (,x ,wg)
             (progn ,@body)
             (incf ,i 4)))))))

(defun clear (buffer &optional (color :black))
  (let ((color (ecase color
                 (:black 0)
                 (:white 255))))
    #+cffi
    (cffi:with-pointer-to-vector-data (ptr buffer)
      (cffi:foreign-funcall "memset" :pointer ptr :int color :size (length buffer)))
    #-cffi
    (fill buffer color)
    buffer))

#+cffi
(progn
  (declaim (inline memset))
  (defun memset (ptr type)
    (cffi:foreign-funcall "memset" :pointer ptr :int 0 :size
                          (etypecase type
                            (integer type)
                            ((or cons symbol) (cffi:foreign-type-size type))))))

#+cffi
(progn
  (declaim (inline memcpy))
  (defun memcpy (dst src type)
    (cffi:foreign-funcall "memcpy" :pointer dst :pointer src :size
                          (etypecase type
                            (integer type)
                            ((or cons symbol) (cffi:foreign-type-size type))))))
