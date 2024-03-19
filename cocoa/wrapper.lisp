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

(defclass window (fb:window)
  ((ptr :initarg :ptr :initform NIL :accessor ptr)
   (view :initarg :view :initform NIL :accessor view)
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
  (fb-int:clean window view cocoa:release)
  (fb-int:clean window ptr cocoa:close)
  (loop while (cocoa:process-event)))

(defun tf-y (y)
  (- (cocoa:rect-size-height (cocoa:cg-display-bounds (cocoa:cg-main-display-id))) y 1))

(defmethod (setf fb:size) (size (window window))
  (objc:with-objects ((rect (cocoa:content-rect-for-frame-rect (ptr window) (cocoa:frame (ptr window)))))
    (incf (cocoa:rect-origin-y rect) (- (cocoa:rect-size-heigh rectt) (cdr size)))
    (setf (cocoa:rect-size rect) (cocoa:make-size (car size) (cdr size)))
    (cocoa:set-frame (ptr window) (cocoa:frame-rect-for-content-rect (ptr window) rect) T)))

(defmethod (setf fb:location) (location (window window))
  (objc:with-objects ((rect (cocoa:frame (view window)))
                      (dummy (cocoa:make-rect (car location) (tf-y (+ (cdr location) (cocoa:rect-size-height rect) -1)) 0 0))
                      (frame (cocoa:frame-rect-for-content-rect (ptr window) dummy)))
    (cocoa:set-frame-origin (ptr window) (cocoa:rect-origin frame))))

(defmethod (setf fb:title) (title (window window))
  (cocoa:set-title (ptr window) title)
  (cocoa:set-miniwindow-title (ptr window) string)
  (setf (fb-int:title window) title))

(defmethod (setf fb:visible-p) (state (window window))
  (if state
      (cocoa:order-front (ptr window) NIL)
      (cocoa:order-out (ptr window) NIL))
  (setf (fb-int:visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window))
  (unless (eq state (fb-int:maximized-p window))
    (cocoa:zoom (ptr window) NIL)
    (setf (fb-int:iconified-p window) state))
  state)

(defmethod (setf fb:iconified-p) (state (window window))
  (when state
    (cocoa:miniaturize (ptr window) NIL)
    (setf (fb-int:iconified-p window) state))
  state)

(defmethod (setf fb:minimum-size) (value (window window))
  (objc:with-objects ((size (cocoa:make-size (max 1 (or (car value) 0))
                                             (max 1 (or (cdr value) 0)))))
    (cocoa:set-content-min-size (ptr window) size)
    (setf (car (fb:minimum-size window)) (max 1 (or (car value) 0)))
    (setf (cdr (fb:minimum-size window)) (max 1 (or (cdr value) 0)))
    value))

(defmethod (setf fb:maximum-size) (value (window window))
  (objc:with-objects ((size (cocoa:make-size (or (car value) most-positive-double-float)
                                             (or (cdr value) most-positive-double-float))))
    (cocoa:set-content-max-size (ptr window) size)
    (setf (car (fb:maximum-size window)) (car value))
    (setf (cdr (fb:maximum-size window)) (cdr value))
    value))

(defmethod (setf fb:focused-p) (value (window window))
  (when value
    (cocoa:nsapp-activate-ignoring-other-apps T)
    (cocoa:make-key-and-order-front (ptr window) NIL)
    (setf (fb-int:focused-p window) value))
  value)

(defmethod (setf fb:borderless-p) (value (window window))
  (let ((mask (cocoa:style-mask (ptr window))))
    (setf mask (if value
                   (set-union '(:borderless) (set-difference mask '(:titled :closable)))
                   (set-union '(:titled :closable) (set-difference mask '(:borderless)))))
    (cocoa:set-style-mask (ptr window) mask))
  (cocoa:make-first-responder (ptr window) (view window))
  (setf (fb-int:floating-p window) value))

(defmethod (setf fb:always-on-top-p) (value (window window))
  ;; TODO: implement always-on-top-p
  )

(defmethod (setf fb:resizable-p) (value (window window))
  (let ((mask (cocoa:style-mask (ptr window))))
    (setf mask (if value
                   (set-difference mask '(:resizable))
                   (set-union mask '(:resizable))))
    (cocoa:set-style-mask (ptr window) mask)
    (cocoa:set-collection-behavior (ptr window) (if value '(:full-screen-primary :managed) '(:full-screen-none)))))

(defmethod (setf fb:floating-p) (value (window window))
  (cocoa:set-level (ptr window) (if value :floating-window-level :normal-window-level))
  (setf (fb-int:floating-p window) value))

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod (setf fb:fullscreen-p) ((value fb:video-mode) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod fb:clipboard ((window window))
  (objc:with-objects ((pasteboard (cocoa:nspasteboard-general-pasteboard)))
    (dolist (type (cocoa:types pasteboard))
      (case type
        (:string (return (cocoa:string-for-type pasteboard :string)))))))

(defmethod (setf fb:clipboard) ((string string) (window window))
  (objc:with-objects ((pasteboard (cocoa:nspasteboard-general-pasteboard)))
    (cocoa:declare-types pasteboard '(:string) NIL)
    (cocoa:set-string pasteboard string :string)))

(defmethod (setf fb:icon) ((value null) (window window))
  ;; TODO: implement icon
  )

(defmethod (setf fb:icon) ((value fb:icon) (window window))
  ;; TODO: implement icon
  )

(defmethod (setf fb:cursor-icon) ((value symbol) (window window))
  ;; TODO: implement cursor-icon
  )

(defmethod (setf fb:cursor-icon) ((value fb:icon) (window window))
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
  (cocoa:nsapp-request-user-attention :informational-request))

(defmethod fb:set-timer ((window window) delay &key repeat)
  ;; TODO: implement set-timer
  )

(defmethod fb:cancel-timer ((window window) timer)
  ;; TODO: implement cancel-timer
  )

(defmethod fb:display ((window window))
  ;; TODO: implement display
  )

(defmethod fb-int:list-displays-backend ((backend (eql :cocoa)))
  ;; TODO: implement list-displays-backend
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
