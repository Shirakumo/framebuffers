(in-package #:org.shirakumo.framebuffers.int)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname* (error "Need compile-file or load."))))
(defvar *windows-table* (make-hash-table :test 'eql))
(defvar *available-backends* ())
(defvar *backend* NIL)

(defgeneric init-backend (backend))
(defgeneric shutdown-backend (backend))
(defgeneric open-backend (backend &key))

(defun static-file (path)
  (merge-pathnames path *here*))

(defun default-title ()
  (format NIL "Framebuffer (~a ~a)" (lisp-implementation-type) (lisp-implementation-version)))

(declaim (inline ptr-int))
(defun ptr-int (ptr)
  (etypecase ptr
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

(defun init ()
  (dolist (backend *available-backends*)
    (handler-case
        (progn (init-backend backend)
               (setf *backend* backend)
               (return-from init backend))
      (error ())))
  (if *available-backends*
      (error "Tried to configure ~{~a~^, ~a~}, but none would start properly." *available-backends*)
      (error "There are no available backends for your system.")))

(defun shutdown ()
  (when *backend*
    (dolist (window (list-windows))
      (ignore-errors (close window)))
    (shutdown-backend (shiftf *backend* NIL))
    (clrhash *windows-table*)))

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
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

(defmethod initialize-instance :after ((window window) &key event-handler)
  (setf (event-handler window) event-handler))

(defclass event-handler ()
  ((window :initform NIL :initarg :window :accessor window)))

(defmethod (setf event-handler) :before ((handler event-handler) (window window))
  (setf (window handler) window))

(defclass dynamic-event-handler (event-handler)
  ((handler :initarg :handler :accessor handler)))

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

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type T :identity T)
    (if (valid-p window)
        (format stream "~dx~d" (width window) (height window))
        (format stream "CLOSED"))))

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

(defmacro define-event-callback (name args)
  `(progn
     (defmethod ,name ((window window) ,@args)
       (,name (event-handler window) ,@args))
     
     (defmethod ,name ((handler event-handler) ,@args))

     (defmethod ,name ((handler dynamic-event-handler) ,@args)
       (funcall (handler handler) ',name (window handler) ,@args))))

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
