(in-package #:org.shirakumo.framebuffers.win32)

(pushnew :win32 fb-int:*available-backends*)

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

(defmacro check-result (form)
  `(unless ,form
     (win32-error :function-name ',(car form))))

(defmethod fb-int:init-backend ((backend (eql :win32)))
  (unless (cffi:foreign-library-loaded-p 'win32:user32)
    (cffi:load-foreign-library 'win32:user32)
    (cffi:load-foreign-library 'win32:gdi32)
    (ignore-errors (cffi:load-foreign-library 'win32:shcore))
    (or (ignore-errors (win32:set-process-dpi-awareness-context :per-monitor-aware-2))
        (ignore-errors (win32:set-process-dpi-awareness-context :per-monitor-aware))
        (ignore-errors (com:check-hresult (win32:set-process-dpi-awareness :per-monitor-dpi-aware)))
        (ignore-errors (win32:set-process-dpi-aware)))))

(defmethod fb-int:shutdown-backend ((backend (eql :win32))))

(defun create-class ()
  (cffi:with-foreign-objects ((class '(:struct win32:window-class)))
    (cffi:foreign-funcall "memset" :pointer class :int 0 :size (cffi:foreign-type-size '(:struct win32:window-class)))
    (cond ((win32:get-class-info (win32:get-module-handle (cffi:null-pointer)) "clframebuffer" class)
           (win32:window-class-class-name class))
          (T
           (setf (win32:window-class-style class) '(:owndc :vredraw :hredraw))
           (setf (win32:window-class-proc class) (cffi:callback handle-event))
           (setf (win32:window-class-cursor class) (win32:load-cursor (cffi:null-pointer) :arrow))
           (setf (win32:window-class-class-name class) "clframebuffer")
           (let ((cls (win32:register-class class)))
             (if (= 0 cls)
                 (win32-error :function-name 'win32:register-class)
                 (cffi:make-pointer cls)))))))

(defmethod fb-int:open-backend ((backend (eql :win32)) &key (title (fb-int:default-title)) location size (visible-p T) event-handler)
  (let* ((screen-w (win32:get-system-metrics :cxscreen))
         (screen-h (win32:get-system-metrics :cyscreen))
         (w (or (car size) screen-w))
         (h (or (cdr size) screen-h))
         (x (or (car location) (round (- screen-w w) 2)))
         (y (or (cdr location) (round (- screen-h h) 2)))
         (style (list :maximizebox :thickframe)))
    (let ((ptr (win32:create-window 0 (create-class) title style x y w h (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))))
      (when (cffi:null-pointer-p ptr)
        (win32-error :function-name 'win32:create-window))
      (make-instance 'window :ptr ptr
                             :size (cons w h)
                             :location (cons x y)
                             :visible-p visible-p
                             :event-handler event-handler))))

(defclass window (fb:window)
  ((ptr :initarg :ptr :accessor ptr)
   (dc :initarg :dc :accessor dc)
   (buffer :reader fb:buffer :initform NIL :accessor buffer)
   (bitmap-info :initform (cffi:foreign-alloc '(:struct win32:bitmap-info)) :accessor bitmap-info)
   (modifiers :initform () :accessor modifiers)
   (surrogate :initform 0 :accessor surrogate)
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
      (win32:show-window ptr :normal))
    (or (ignore-errors
         (cffi:with-foreign-objects ((x :uint) (y :uint))
           (win32:get-dpi-for-monitor (win32:monitor-from-window ptr 2) 0 x y)
           (setf (car (content-scale window)) (/ (cffi:mem-ref x :uint) 96))
           (setf (car (content-scale window)) (/ (cffi:mem-ref y :uint) 96))))
        (progn
          (setf (car (content-scale window)) (/ (win32:get-device-caps (dc window) :logpixelsx) 96))
          (setf (car (content-scale window)) (/ (win32:get-device-caps (dc window) :logpixelsy) 96))))))

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

(defun get-window-style (window)
  '(:clipsiblings
    :clipchildren
    :sysmenu
    :minimizebox
    :caption
    :maximizebox
    :thickframe))

(defmethod (setf fb:size) (size (window window))
  (with-rect (rect 0 0 (car size) (cdr size))
    (win32:adjust-window-rect rect (get-window-style window) NIL)
    (let ((w (- (win32:rect-right rect) (win32:rect-left rect)))
          (h (- (win32:rect-bottom rect) (win32:rect-top rect))))
      (win32:set-window-pos (ptr window) (cffi:null-pointer) 0 0 w h '(:noactivate :nozorder :nomove :noownerzorder))
      (setf (car (size window)) w)
      (setf (cdr (size window)) h)
      size)))

(defmethod (setf fb:location) (location (window window))
  (with-rect (rect (car location) (cdr location) 0 0)
    (win32:adjust-window-rect rect (get-window-style window) NIL)
    (let ((x (win32:rect-left rect))
          (y (win32:rect-top rect)))
      (win32:set-window-pos (ptr window) (cffi:null-pointer) x y 0 0 '(:noactivate :nozorder :nosize))
      (setf (car (location window)) x)
      (setf (cdr (location window)) y))
    location))

(defmethod (setf fb:title) (title (window window))
  (win32:set-window-text (ptr window) title)
  title)

(defmethod (setf fb:visible-p) (state (window window))
  (win32:show-window (ptr window) (if state :showna :hide))
  (setf (visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window))
  (win32:show-window (ptr window) (if state :maximize :restore))
  (setf (maximized-p window) state))

(defmethod (setf fb:iconified-p) (state (window window))
  (win32:show-window (ptr window) (if state :minimize :restore))
  (setf (iconified-p window) state))

(defmethod fb:clipboard-string ((window window))
  ;; TODO: get clipboard string
  )

(defmethod (setf fb:clipboard-string) (string (window window))
  ;; TODO: set clipboard string
  )

(defmethod fb:request-attention ((window window))
  (win32:flash-window (ptr window) T))

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
    (setf (ldb (byte 16  0) xy) x)
    (setf (ldb (byte 16 16) xy) y)
    xy))

(defun dec32 (xy)
  (values (ldb (byte 16  0) xy)
          (ldb (byte 16 16) xy)))

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (with-rect (rect x y w h)
    (win32:invalidate-rect (ptr window) rect T)
    (win32:stretch-di-bits (dc window) x y w h x y w h
                           (static-vectors:static-vector-pointer (buffer window))
                           (bitmap-info window) 0 #x00CC0020)))

(defmethod fb:process-events ((window window) &key timeout)
  (cffi:with-foreign-objects ((msg '(:struct win32:message)))
    (flet ((poll-events ()
             (loop while (win32:peek-message msg (ptr window) 0 0 1)
                   do (case (win32:message-type msg)
                        (:quit
                         (fb:shutdown))
                        (T
                         (win32:translate-message msg)
                         (win32:dispatch-message msg))))))
      (etypecase timeout
        (null
         (poll-events))
        ((eql T)
         (loop while (and (ptr window) (not (close-requested-p window)))
               do (win32:msg-wait-for-multiple-objects 0 (cffi:null-pointer) NIL (truncate 1000) #xFFFF)
                  (poll-events)))
        (real
         (win32:msg-wait-for-multiple-objects 0 (cffi:null-pointer) NIL (truncate 1000) #xFFFF)
         (poll-events))))))

(cffi:defcallback handle-event :ssize ((ptr :pointer) (message win32::message-type) (wparam :size) (lparam :size))
  (let ((window (fb-int:ptr-window ptr)))
    (block NIL
      (when window
        (case message
          (:nccreate
           (ignore-errors (win32:enable-non-client-dpi-scaling ptr)))
          (:paint
           (fb:window-refreshed window))
          (:close
           (setf (fb:close-requested-p window) T)
           (fb:window-closed window)
           (return 0))
          (:syscommand
           ;; TODO: prevent power save in fullscreen
           )
          (:inputlangchange
           ;; TODO: update key names
           )
          ((:keydown :syskeydown :keyup :syskeyup)
           ;; TODO: handle key repeats
           (let ((scancode (logand (ldb (byte 16 16) lparam) #x1FF))
                 (action (if (logtest #x8000 (ldb (byte 16 16) lparam)) :release :press))
                 (modifiers (update-modifiers window)))
             (when (= #x000 scancode) (setf scancode (win32:map-virtual-key wparam 0)))
             (when (= #x054 scancode) (setf scancode #x137))
             (when (= #x146 scancode) (setf scancode #x045))
             (when (= #x136 scancode) (setf scancode #x036))
             (cond ((= wparam (cffi:foreign-enum-value 'win32:key :CONTROL))
                    (cond ((logtest (ldb (byte 16 16) lparam) #x0100)
                           (fb:key-changed window :right-control scancode :press modifiers))
                          (T ;; Hack to handle AltGr sending LCTRL + RALT
                           (cffi:with-foreign-objects ((msg '(:struct win32:message)))
                             (let ((time (win32:get-message-time)))
                               (unless (and (win32:peek-message msg ptr 0 0 0)
                                            (find (win32:message-type msg) '(:keydown :syskeydown :keyup :syskeyup))
                                            (= (win32:message-wparameter msg) (cffi:foreign-enum-value 'win32:key :MENU))
                                            (logtest (ldb (byte 16 16) (win32:message-lparameter msg)) #x0100)
                                            (= time (win32:message-time msg)))
                                 (fb:key-changed window :left-control scancode :press modifiers)))))))
                   ((= wparam (cffi:foreign-enum-value 'win32:key :PROCESSKEY)))
                   ((and (= wparam (cffi:foreign-enum-value 'win32:key :SHIFT)) (eql action :release))
                    (fb:key-changed window :left-shift scancode action modifiers)
                    (fb:key-changed window :right-shift scancode action modifiers))
                   ((= wparam (cffi:foreign-enum-value 'win32:key :SNAPSHOT))
                    (fb:key-changed window (translate-keycode scancode) scancode :press modifiers)
                    (fb:key-changed window (translate-keycode scancode) scancode :release modifiers))
                   (T
                    (fb:key-changed window (translate-keycode scancode) scancode action modifiers)))))
          ((:char :syschar)
           (cond ((<= #xD800 wparam #xDBFF)
                  (setf (surrogate window) wparam))
                 ((<= #xDC00 wparam #xDFFF)
                  (fb:string-entered window (string (code-char (+ (ash (- (surrogate window) #xD800) 10)
                                                                  (- wparam #xDC00)
                                                                  #x10000))))
                  (setf (surrogate window) NIL))
                 (T
                  (fb:string-entered window (string (code-char wparam)))
                  (setf (surrogate window) NIL)))
           (return 0))
          (:unichar
           (cond ((= wparam #xFFFF)
                  (return 1))
                 (T
                  (fb:string-entered window (string (code-char wparam)))
                  (return 0))))
          ((:lbuttonup :lbuttondown :lbuttondblclk)
           (let ((action (case message (:lbuttonup :press) (:lbuttondown :release) (:lbuttondblclk :double-click))))
             (fb:mouse-button-changed window :left action (update-modifiers window)))
           (return 0))
          ((:rbuttonup :rbuttondown :rbuttondblclk)
           (let ((action (case message (:rbuttonup :press) (:rbuttondown :release) (:rbuttondblclk :double-click))))
             (fb:mouse-button-changed window :right action (update-modifiers window)))
           (return 0))
          ((:mbuttonup :mbuttondown :mbuttondblclk)
           (let ((action (case message (:mbuttonup :press) (:mbuttondown :release) (:mbuttondblclk :double-click))))
             (fb:mouse-button-changed window :middle action (update-modifiers window)))
           (return 0))
          ((:xbuttonup :xbuttondown :xbuttondblclk)
           (let ((action (case message (:xbuttonup :press) (:xbuttondown :release) (:xbuttondblclk :double-click))))
             (fb:mouse-button-changed window (ldb (byte 16 16) wparam) action (update-modifiers window)))
           (return 0))
          (:mousewheel
           (fb:mouse-scrolled window 0 (/ (ldb (byte 16 16) wparam) 120))
           (return 0))
          (:mousehwheel
           (fb:mouse-scrolled window (/ (ldb (byte 16 16) wparam) -120) 0)
           (return 0))
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
             (fb:mouse-moved window x y))
           (return 0))
          (:mouseleave
           (setf (mouse-entered-p window) NIL)
           (fb:mouse-entered window NIL)
           (return 0))
          (:move
           (multiple-value-bind (x y) (dec32 lparam)
             (setf (car (location window)) x)
             (setf (cdr (location window)) y)
             (fb:window-moved window x y))
           (return 0))
          (:size
           (unless (iconified-p window)
             (multiple-value-bind (w h) (dec32 lparam)
               (update-buffer window w h)
               (fb:window-resized window w h)
               (fb:window-refreshed window)))
           (let ((iconified (= wparam 1)))
             (unless (eq iconified (iconified-p window))
               (setf (iconified-p window) iconified)
               (fb:window-iconified window iconified)))
           (let ((maximized (= wparam 2)))
             (unless (eq maximized (maximized-p window))
               (setf (maximized-p window) maximized)
               (fb:window-maximized window maximized)))
           (return 0))
          (:setfocus
           (fb:window-focused window T)
           (return 0))
          (:killfocus
           (fb:window-focused window NIL)
           (return 0))
          (:dpichanged
           (multiple-value-bind (x y) (dec32 wparam)
             (setf (car (content-scale window)) (/ x 96))
             (setf (cdr (content-scale window)) (/ y 96))
             (fb:content-scale-changed window (/ x 96) (/ y 96))))
          (:dropfiles
           ;; TODO: implement dnd
           )))
      (return (win32:def-window-proc ptr message wparam lparam)))))
