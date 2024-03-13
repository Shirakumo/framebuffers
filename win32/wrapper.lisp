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
    (setf (win32:window-class-cursor class) (win32:load-cursor (cffi:null-pointer) :arrow))
    (setf (win32:window-class-class-name class) "framebuffer")
    (win32:register-class class)
    (let ((ptr (win32:create-window 0 "framebuffer" title style x y w h 0 0 0 0)))
      (when (cffi:null-pointer-p ptr)
        (win32-error :function-name 'win32:create-window))
      (make-instance 'window :ptr ptr
                             :class class
                             :size (cons w h)
                             :location (cons x y)
                             :visible-p visible-p))))

(defclass window (fb:window)
  ((ptr :initarg :ptr :accessor ptr)
   (dc :initarg :dc :accessor dc)
   (class :initarg :class :accessor window-class)
   (buffer :reader fb:buffer :accessor buffer)
   (bitmap-info :initform (cffi:foreign-alloc '(:struct win32:bitmap-info)) :accessor bitmap-info)
   (modifiers :initform () :accessor modifiers)
   (close-requested-p :initform NIL :initarg :close-requested-p :accessor fb:close-requested-p :accessor close-requested-p)
   (size :initform (cons 1 1) :initarg :size :reader fb:size :accessor size)
   (location :initform (cons 0 0) :initarg :location :reader fb:location :accessor location)
   (title :initform NIL :initarg :title :reader fb:title :accessor title)
   (visible-p :initform NIL :initarg :visible-p :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :initarg :maximized-p :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :initarg :iconified-p :reader fb:iconified-p :accessor iconified-p)
   (mouse-entered-p :initform NIL :initarg :mouse-entered-p :accessor mouse-entered-p)
   (content-scale :initform (cons 1 1) :initarg :content-scale :reader fb:content-scale :accessor content-scale)))

(defmethod initialize-instance :after ((window window) &key)
  (let ((ptr (ptr window))
        (bi (bitmap-info window)))
    (setf (fb-int:ptr-window ptr) window)
    (win32:set-window ptr :userdata ptr)
    (setf (dc window) (win32:get-dc ptr))
    (setf (win32:bitmap-info-size bi) (- (cffi:foreign-type-size '(:struct win32:bitmap-info)) 16))
    (setf (win32:bitmap-info-planes bi) 1)
    (setf (win32:bitmap-info-bit-count bi) 32)
    (setf (win32:bitmap-info-compression bi) 3)
    (setf (win32:bitmap-info-alpha-mask bi) #xFF000000)
    (setf (win32:bitmap-info-red-mask bi)   #x00FF0000)
    (setf (win32:bitmap-info-green-mask bi) #x0000FF00)
    (setf (win32:bitmap-info-blue-mask bi)  #x000000FF)
    (update-buffer window (fb:width window) (fb:height window))
    (when (visible-p window)
      (win32:show-window ptr :normal))))

(defmethod fb:valid-p ((window window))
  (not (null (ptr window))))

(defmethod fb:close ((window window))
  (when (bitmap-info window)
    (cffi:foreign-free (bitmap-info window))
    (setf (bitmap-info window) NIL))
  (when (dc window)
    (win32:release-dc (ptr window) (dc window))
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

(defun update-buffer (window w h)
  (setf (buffer window) (fb-int:resize-buffer w h (buffer window) (car (size window)) (cdr (size window))))
  (setf (win32:bitmap-info-width (bitmap-info window)) w)
  (setf (win32:bitmap-info-height (bitmap-info window)) h)
  (setf (car (size window)) w)
  (setf (cdr (size window)) h))

(defun update-modifiers (window)
  (setf (modifiers window) ())
  (when (< 0 (logand #x8000 (win32:get-key-state :shift)))
    (push :shift (modifiers window)))
  (when (< 0 (logand #x8000 (win32:get-key-state :control)))
    (push :control (modifiers window)))
  (when (< 0 (logand #x8000 (win32:get-key-state :menu)))
    (push :alt (modifiers window)))
  (when (or (< 0 (logand #x8000 (win32:get-key-state :lwin)))
            (< 0 (logand #x8000 (win32:get-key-state :rwin))))
    (push :super (modifiers window)))
  (when (< 0 (logand #x1 (win32:get-key-state :capital)))
    (push :caps-lock (modifiers window)))
  (when (< 0 (logand #x1 (win32:get-key-state :numlock)))
    (push :num-lock (modifiers window)))
  (modifiers window))

(defun enc32 (x y)
  (let ((xy 0))
    (setf (ldb (byte 32  0) xy) x)
    (setf (ldb (byte 32 32) xy) y)
    xy))

(defun dec32 (xy)
  (values (ldb (byte 32  0) xy)
          (ldb (byte 32 32) xy)))

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (with-rect (rect x y w h)
    (win32:invalidate-rect (ptr window) rect T)
    (win32:send-message (ptr window) :paint (enc32 x y) (enc32 w h)))
  (when sync
    ;; TODO: sync
    ))

(defmethod fb:process-events ((window window) &key timeout))

(cffi:defcallback handle-event :ssize ((ptr :pointer) (message win32::message-type) (wparam :size) (lparam :size))
  (let ((window (fb-int:ptr-window ptr)))
    (flet ((default ()
             (return-from handle-event (win32:def-window-proc ptr message wparam lparam))))
      (unless window
        (default))
      (case message
        (:nccreate
         (ignore-errors (win32:enable-non-client-dpi-scaling ptr))
         (default))
        (:paint
         (multiple-value-bind (x y) (dec32 wparam)
           (multiple-value-bind (w h) (dec32 lparam)
             (win32:stretch-di-bits (dc window) x y w h x y w h
                                    (static-vectors:static-vector-pointer (buffer window))
                                    (bitmap-info window) 0 #x00CC0020))))
        (:close
         (setf (fb:close-requested-p window) T)
         (fb:window-closed window))
        (:destroy
         (fb:close window))
        ((:keydown :syskeydown :keyup :syskeyup)
         (update-modifiers window)
         ;; TODO: key translation
         )
        ((:char :syschar)
         ;; TODO: unicode translation
         )
        (:unichar
         (if (= wparam #xFFFF)
             1
             (fb:string-entered window (string (code-char wparam)))))
        ((:lbuttonup :lbuttondown :lbuttondblclk)
         (let ((action (case message (:lbuttonup :press) (:lbuttondown :release) (:lbuttondblclk :double-click))))
           (fb:mouse-button-changed window :left action (update-modifiers window))))
        ((:rbuttonup :rbuttondown :rbuttondblclk)
         (let ((action (case message (:rbuttonup :press) (:rbuttondown :release) (:rbuttondblclk :double-click))))
           (fb:mouse-button-changed window :right action (update-modifiers window))))
        ((:mbuttonup :mbuttondown :mbuttondblclk)
         (let ((action (case message (:mbuttonup :press) (:mbuttondown :release) (:mbuttondblclk :double-click))))
           (fb:mouse-button-changed window :middle action (update-modifiers window))))
        ((:xbuttonup :xbuttondown :xbuttondblclk)
         (let ((action (case message (:xbuttonup :press) (:xbuttondown :release) (:xbuttondblclk :double-click))))
           (fb:mouse-button-changed window (ldb (byte 16 16) wparam) action (update-modifiers window))))
        (:mousewheel
         (fb:mouse-scrolled window 0 (/ wparam 0)))
        (:mousehwheel
         (fb:mouse-scrolled window (/ wparam -0) 0))
        (:mousemove
         (unless (mouse-entered-p window)
           (setf (mouse-entered-p window) T)
           (cffi:with-foreign-objects ((track '(:struct win32:track-mouse-event)))
             (setf (win32:track-mouse-event-size track) (cffi:foreign-type-size '(:struct win32:track-mouse-event)))
             (setf (win32:track-mouse-event-flags track) 2)
             (setf (win32:track-mouse-event-track track) ptr)
             (setf (win32:track-mouse-event-hover-time track) 0)
             (win32:track-mouse-event track)))
         (multiple-value-bind (x y) (dec32 lparam)
           (fb:mouse-moved window x y)))
        (:mouseleave
         (setf (mouse-entered-p window) NIL)
         (fb:mouse-entered window NIL))
        (:size
         (unless (iconified-p window)
           (multiple-value-bind (w h) (dec32 lparam)
             (update-buffer window w h)
             (fb:window-resized window w h))))
        (:setfocus
         (fb:window-focused window T))
        (:killfocus
         (fb:window-focused window NIL))
        (T
         (default))))))
