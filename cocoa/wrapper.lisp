(in-package #:org.shirakumo.framebuffers.cocoa)

(pushnew :cocoa fb-int:*available-backends*)

(define-condition cocoa-error (fb:framebuffer-error objc:foundation-error)
  ())

(cffi:defcallback %foundation-error :void ((exception :pointer))
  (objc:foundation-error exception 'cocoa-error))

(defmethod fb-int:init-backend ((backend (eql :cocoa)))
  (objc:init)
  (org.shirakumo.cocoas.cffi:set-uncaught-exception-handler
   (cffi:callback '%foundation-error)))

(defmethod fb-int:shutdown-backend ((backend (eql :cocoa)))
  (objc:shutdown))

(defmethod fb-int:open-backend ((backend (eql :cocoa)) &key size location title (visible-p T))
  (cocoa:nsapplication-shared-application)
  (cocoa:nsapp-set-activation-policy :regular)
  (let* ((screen (cocoa:frame (cocoa:nsscreen-main-screen)))
         (w (or (car size) (cocoa:rect-w screen)))
         (h (or (cdr size) (cocoa:rect-h screen)))
         (x (or (car location) (truncate (- (cocoa:rect-w screen) w) 2)))
         (y (or (cdr location) (truncate (- (cocoa:rect-h screen) h) 2)))
         (rect (cocoa:make-rect x y w h))
         (window (cocoa:init-with-content-rect (cocoa:alloc "FBWindow") rect '(:closable :titled) :buffered NIL)))
    (make-instance 'window :ptr window
                           :size (cons w h)
                           :location (cons x y)
                           :title (or title (fb-int:default-title)))))

(defmacro define-objc-class (name super &body methods)
  `(objc::define-objc-class ,name ,super
     ,@(loop for (name rettype args . body) in methods
             collect `(,name ,rettype ,args
                             (let ((window (fb-int:ptr-window self)))
                               (declare (ignorable window))
                               ,@body)))))

(trivial-indent:define-indentation define-objc-class
    (4 4 &rest (&whole 2 6 6 &body)))

(define-objc-class "FBWindow" "NSWindow"
  (can-become-key-window :bool () T)
  (can-become-main-window :bool () T)
  (window-should-close/ :bool ((sender :id))
    (fb:window-closed window)
    NIL)
  (window-did-resize/ :void ((notification :pointer))
    (let ((max (cocoa:is-zoomed self)))
      (unless (eql max (fb:maximized-p window))
        (fb:window-maximized window max)))
    (objc:with-objects ((content-rect (cocoa:frame (view window)))
                        (rect (cocoa:convert-rect-to-backing (view window) content-rect)))
      (fb:window-resized window (cocoa:rect-w rect) (cocoa:rect-h rect))))
  (window-did-move/ :void ((notification :pointer))
    (objc:with-objects ((rect (cocoa:content-rect-for-frame-rect self (cocoa:frame self))))
      (fb:window-moved window
                       (cocoa:rect-x rect)
                       (tf-y (+ (cocoa:rect-y rect) (cocoa:rect-h rect) -1)))))
  (window-did-miniaturize/ :void ((notification :pointer))
    (fb:window-iconified window T))
  (window-did-deminiaturize/ :void ((notification :pointer))
    (fb:window-iconified window NIL))
  (window-did-become-key/ :void ((notification :pointer)))
  (window-did-resign-key/ :void ((notification :pointer))))

(define-objc-class "FBContentView" "NSView"
  (can-become-key-view :bool () T)
  (accepts-first-responder :bool () T)
  (wants-update-layer :bool () T)
  (accepts-first-mouse/ :bool ((event :pointer)) T)
  (update-layer :void ()
    (fb:window-refreshed window))
  (cursor-update/ :void ((event :pointer)))
  (mouse-down/ :void ((event :pointer)))
  (mouse-dragged/ :void ((event :pointer)))
  (mouse-up/ :void ((event :pointer)))
  (mouse-moved/ :void ((event :pointer)))
  (right-mouse-down/ :void ((event :pointer)))
  (right-mouse-dragged/ :void ((event :pointer)))
  (right-mouse-up/ :void ((event :pointer)))
  (other-mouse-down/ :void ((event :pointer)))
  (other-mouse-dragged/ :void ((event :pointer)))
  (other-mouse-up/ :void ((event :pointer)))
  (mouse-exited/ :void ((event :pointer)))
  (mouse-entered/ :void ((event :pointer)))
  (scroll-wheel/ :void ((event :pointer)))
  (view-did-change-backing-properties :void ())

  (draw-rect/ :void ((rect :pointer))
    (destructuring-bind (w . h) (fb:size window)
      (let* ((buffer (fb:buffer window))
             (context (cocoa:cgcontext (cocoa:nsgraphicscontext-current-context)))
             (space (cocoa:cg-color-space-create-device-rgb))
             (provider (cocoa:cg-data-provider-create-with-data (cffi:null-pointer)
                                                                (static-vectors:static-vector-pointer buffer)
                                                                (length buffer)
                                                                (cffi:null-pointer)))
             (image (cocoa:cg-image-create w h 8 32 (* w 4) space '(:none-skip-first :byte-order-32-little) provider (cffi:null-pointer) NIL :default)))
        (cocoa:cg-color-space-release space)
        (cocoa:cg-data-provider-release provider)
        (cocoa:cg-context-draw-image context rect image)
        (cocoa:cg-image-release image))))

  (update-tracking-areas :void ())
  (key-down/ :void ((event :pointer)))
  (flags-changed/ :void ((event :pointer)))
  (key-up/ :void ((event :pointer)))
  (dragging-entered/ :id ((sender :id)))
  (perform-drag-operation/ :bool ((sender :id)))
  (has-marked-text :bool ())
  (marked-range :id ())
  (selected-range :id ())
  (set-marked-text/selected-range/replacement-range/ :void ((string :id) (selected-range :id) (replacement-range :id)))
  (unmark-text :void ())
  (valid-attributes-for-marked-text :pointer ())
  (attributed-substring-for-proposed-range/actual-range/ :pointer ((range :id) (actual-range :pointer)))
  (character-index-for-point :uint ((point :id)) 0)
  (first-rect-for-character-range/actual-range/ :pointer ((range :id) (actual-range :pointer)))
  (insert-text/replacement-range/ :void ((string :id) (replacement-range :id))))

(defclass window (fb:window)
  ((ptr :initarg :ptr :initform NIL :accessor ptr)
   (view :initarg :view :initform NIL :accessor view)
   (buffer :initarg :buffer :initform NIL :reader fb:buffer :accessor buffer)))

(defmethod initialize-instance :after ((window window) &key)
  (let ((ptr (ptr window))
        (view (cocoa:init (cocoa:alloc "FBContentView"))))
    (setf (fb-int:ptr-window ptr) window)
    (setf (fb-int:ptr-window view) window)
    (cocoa:set-release-when-closed ptr NIL)
    (cocoa:set-opaque ptr T)
    (cocoa:set-background-color ptr (cocoa:nscolor-clear-color))
    (cocoa:set-accepts-mouse-moved-events ptr T)
    (cocoa:set-title ptr (fb:title window))
    (cocoa:set-content-view ptr view)
    (cocoa:make-first-responder-view ptr view)
    (cocoa:nsapp-activate-ignoring-other-apps T)))

(defmethod fb:valid-p ((window window))
  (not (null (ptr window))))

(defmethod fb:close ((window window))
  (setf (fb-int:ptr-window (ptr window)) NIL)
  (setf (fb-int:ptr-window (view window)) NIL)
  (fb-int:clean window view objc:release)
  (fb-int:clean window ptr cocoa:close)
  (loop while (objc:process-event)))

(defun tf-y (y)
  (- (cocoa:rect-h (cocoa:cg-display-bounds (cocoa:cg-main-display-id))) y 1))

(defmethod (setf fb:size) (size (window window))
  (objc:with-objects ((rect (cocoa:content-rect-for-frame-rect (ptr window) (cocoa:frame (ptr window)))))
    (incf (cocoa:rect-y rect) (- (cocoa:rect-h rect) (cdr size)))
    (setf (cocoa:rect-w rect) (car size))
    (setf (cocoa:rect-h rect) (cdr size))
    (cocoa:set-frame (ptr window) (cocoa:frame-rect-for-content-rect (ptr window) rect) T)))

(defmethod (setf fb:location) (location (window window))
  (objc:with-objects ((rect (cocoa:frame (view window)))
                      (dummy (cocoa:make-rect (car location) (tf-y (+ (cdr location) (cocoa:rect-h rect) -1)) 0 0))
                      (frame (cocoa:frame-rect-for-content-rect (ptr window) dummy)))
    (cocoa:set-frame-origin (ptr window) (cocoa:rect frame))))

(defmethod (setf fb:title) (title (window window))
  (cocoa:set-title (ptr window) title)
  (cocoa:set-miniwindow-title (ptr window) title)
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

(defmethod fb:clipboard ((window window))
  (objc:with-objects ((pasteboard (cocoa:nspasteboard-general-pasteboard)))
    (loop for type across (cocoa:types pasteboard)
          do (cond ((cffi:pointer-eq type cocoa:paste-string)
                    (return (cocoa:string-for-type pasteboard cocoa:paste-string)))))))

(defmethod (setf fb:clipboard) ((string string) (window window))
  (objc:with-objects ((pasteboard (cocoa:nspasteboard-general-pasteboard)))
    (cocoa:declare-types pasteboard (list cocoa:paste-string) NIL)
    (cocoa:set-string pasteboard string cocoa:paste-string)))

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
  (cocoa:set-needs-display-in-rect (view window) (cocoa:make-rect x y w h)))

(defmethod fb:process-events ((window window) &key timeout)
  (let ((s (etypecase timeout
             ((eql T) 1.0d0)
             (real (float timeout 0d0))
             (null 0.0d0))))
    (loop while (and (ptr window) (not (fb:close-requested-p window)))
          do (objc:process-event :timeout s)
             (loop while (objc:process-event))
             (unless (eql T timeout)
               (return)))))

(defmethod fb:request-attention ((window window))
  (cocoa:nsapp-request-user-attention :informational-request))

(defmethod fb:set-timer ((window window) delay &key repeat)
  ;; TODO: implement set-timer
  )

(defmethod fb:cancel-timer ((window window) timer)
  ;; TODO: implement cancel-timer
  )

(defmethod fb:display ((window window))
  (typecase (fb:fullscreen-p window)
    (null (call-next-method))
    (display (fb:fullscreen-p window))
    (video-mode (fb:display (fb:fullscreen-p window)))))

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

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod (setf fb:fullscreen-p) ((value fb:video-mode) (window window))
  ;; TODO: implement fullscreen-p
  )
