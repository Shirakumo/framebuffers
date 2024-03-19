(in-package #:org.shirakumo.framebuffers.cocoa)

(pushnew :cocoa fb-int:*available-backends*)

(define-condition cocoa-error (fb:framebuffer-error)
  ()
  (:report (lambda (c s) (format s ""))))

(defmethod fb-int:init-backend ((backend (eql :cocoa)))
  (objc:init))

(defmethod fb-int:shutdown-backend ((backend (eql :cocoa)))
  (objc:shutdown))

(defmethod fb-int:open-backend ((backend (eql :cocoa)) &key size location title (visible-p T))
  (cocoa:nsapplication-shared-application)
  (cocoa:nsapp-set-activation-policy :regular)
  (let* ((screen (cocoa:frame (cocoa:nsscreen-main-screen)))
         (w (or (car size) (cocoa:rect-width screen)))
         (h (or (cdr size) (cocoa:rect-height screen)))
         (x (or (car location) (truncate (- (cocoa:rect-width screen) w) 2)))
         (y (or (cdr location) (truncate (- (cocoa:rect-height screen) h) 2)))
         (rect (cocoa:make-rect x y w h))
         (window (cocoa:init-with-content-rect (cocoa:alloc "NSWindow") rect '(:closable :titled) :buffered NIL)))
    (make-instance 'window :ptr window
                           :size (cons w h)
                           :location (cons x y))))

(defmethod fb-int:list-displays-backend ((backend (eql :cocoa)))
  ;; TODO: implement list-displays-backend
  )

(defclass window (fb:window)
  ((ptr :initarg :ptr :initform NIL :accessor ptr)
   (buffer :initarg :buffer :initform NIL :reader fb:buffer :accessor buffer)))

(defmethod initialize-instance :after ((window window) &key)
  (let ((ptr (ptr window)))
    (cocoa:set-release-when-closed ptr NIL)
    (cocoa:set-opaque ptr T)
    (cocoa:set-background-color ptr (cocoa:nscolor-clear-color))
    (cocoa:set-accepts-mouse-moved-events ptr T)
    (cocoa:set-title ptr (or title (fb-int:default-title)))
    (cocoa:nsapp-activate-ignoring-other-apps T)))

(defmethod fb:valid-p ((window window))
  (not (null (ptr window))))

(defmethod fb:close ((window window))
  ;; TODO: implement close
  )

(defmethod (setf fb:size) (size (window window))
  ;; TODO: implement size
  )

(defmethod (setf fb:location) (location (window window))
  ;; TODO: implement location
  )

(defmethod (setf fb:title) (title (window window))
  ;; TODO: implement title
  )

(defmethod (setf fb:visible-p) (state (window window))
  ;; TODO: implement visible-p
  )

(defmethod (setf fb:maximized-p) (state (window window))
  ;; TODO: implement maximized-p
  )

(defmethod (setf fb:iconified-p) (state (window window))
  ;; TODO: implement iconified-p
  )

(defmethod (setf fb:minimum-size) (value (window window))
  ;; TODO: implement minimum-size
  )

(defmethod (setf fb:maximum-size) (value (window window))
  ;; TODO: implement maximum-size
  )

(defmethod (setf fb:focused-p) (value (window window))
  ;; TODO: implement focused-p
  )

(defmethod (setf fb:borderless-p) (value (window window))
  ;; TODO: implement borderless-p
  )

(defmethod (setf fb:always-on-top-p) (value (window window))
  ;; TODO: implement always-on-top-p
  )

(defmethod (setf fb:resizable-p) (value (window window))
  ;; TODO: implement resizable-p
  )

(defmethod (setf fb:floating-p) (value (window window))
  ;; TODO: implement floating-p
  )

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod (setf fb:fullscreen-p) ((value fb:video-mode) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod fb:clipboard ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard) (string (window window))
  ;; TODO: implement clipboard setting
  )

(defmethod (setf fb:icon) (value (window window))
  ;; TODO: implement icon
  )

(defmethod (setf fb:cursor-icon) (value (window window))
  ;; TODO: implement cursor-icon
  )

(defmethod (setf fb:cursor-state) (value (window window))
  ;; TODO: implement cursor-state
  )

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  ;; TODO: implement swap-buffers
  )

(defmethod fb:process-events ((window window) &key timeout)
  ;; TODO: implement process-events
  )

(defmethod fb:request-attention ((window window))
  ;; TODO: implement request-attention
  )

(defmethod fb:set-timer ((window window) delay &key repeat)
  ;; TODO: implement set-timer
  )

(defmethod fb:cancel-timer ((window window) timer)
  ;; TODO: implement cancel-timer
  )

(defmethod fb:display ((window window))
  ;; TODO: implement display
  )

(defclass display (fb:display)
  ())

(defstruct (video-mode (:include fb:video-mode)))

(defmethod fb:video-modes ((display display))
  ;; TODO: implement video-modes
  )

(defmethod fb:video-mode ((display display))
  ;; TODO: implement video-mode
  )
