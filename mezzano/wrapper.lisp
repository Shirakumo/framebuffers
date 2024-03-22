(in-package #:org.shirakumo.framebuffers.mezzano)

(defvar *default-display* NIL)

(pushnew :mezzano fb-int:*available-backends*)

(define-condition mezzano-error (fb:framebuffer-error)
  ((original-error :initarg :original-error :reader original-error))
  (:report (lambda (c s) (format s "~a" (original-error c)))))

(defmacro with-resignalling (&body body)
  `(handler-bind ((error
                    (lambda (e)
                      (error 'mezzano-error :original-error e))))
     ,@body))

(defmethod fb-int:init-backend ((backend (eql :mezzano)))
  (unless *default-display*
    (multiple-value-bind (w h) (mezzano.gui.compositor::screen-dimensions)
      (let ((mode (make-video-mode :width w :height h :refresh-rate 60)))
        (setf *default-display* (make-instance 'display
                                               :primary-p T
                                               :title "Default"
                                               :id "-"
                                               :size (cons w h)
                                               :video-mode mode
                                               :video-modes (list mode)))))))

(defmethod fb-int:shutdown-backend ((backend (eql :mezzano))))

(defmethod fb-int:open-backend ((backend (eql :mezzano)) &rest args &key &allow-other-keys)
  (let ((w (fb:width *default-display*))
        (h (fb:height *default-display*)))
    (apply #'make-instance 'window 
           :mailbox (mezzano.supervisor:make-fifo 50)
           :thread (mezzano.supervisor:current-thread)
           :buffer (mezzano.gui:make-surface w h)
           :size (or size (cons w h))
           args)))

(defmethod fb-int:list-displays-backend ((backend (eql :mezzano)))
  (list *default-display*))

(defclass window (fb:window mezzano.gui.compositor:window)
  ((buffer :initform NIL :reader fb:buffer :accessor buffer)
   (original-size :initform (cons NIL NIL) :accessor original-size)
   (original-location :initform (cons NIL NIL) :accessor original-location)
   (frame :initform NIL :accessor frame)
   (timers :initform () :accessor timers)))

(defmethod initialize-instance :after ((window window) &key title)
  (setf (frame window) (make-instance 'mezzano.gui.widgets:frame
                                      :framebuffer (mezzano.gui.compositor:window-buffer window)
                                      :title (or title (fb-int:default-title))
                                      :close-button-p T
                                      :resizablep (fb:resizable-p window)
                                      :damage-function (mezzano.gui.widgets:default-damage-function window)
                                      :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
  (mezzano.gui.compositor::submit-compositor-event (make-instance 'mezzano.gui.compositor:window-create-event
                                                                  :window window
                                                                  :initial-z-order :top)))

(defun update-buffer (window w h)
  (multiple-value-bind (left right top bottom) (if (or (fb:fullscreen-p window)
                                                       (fb:borderless-p window))
                                                   (values 0 0 0 0)
                                                   (mezzano.gui.widgets:frame-size (frame window)))
    (let ((new-framebuffer (mezzano.gui:make-surface (+ w left right) (+ h top bottom))))
      (mezzano.gui.widgets:resize-frame (frame window) new-framebuffer)
      (mezzano.gui.compositor:resize-window (window window) new-framebuffer)))
  (fb-int:resize-buffer w h (buffer window) (fb:width window) (fb:height window))
  (setf (car (fb-int:size window)) w)
  (setf (cdr (fb-int:size window)) h))

(defmethod fb:valid-p ((window window))
  (not (null (frame window))))

(defmethod fb:close ((window window))
  (mezzano.gui.compositor:close-window window)
  (setf (frame window) NIL))

(defmethod (setf fb:size) (new-size (window window))
  (unless (equal new-size (fb:size window))
    (update-buffer window (car new-size) (cdr new-size))
    (fb:window-resized window (car new-size) (cdr new-size))
    (fb:window-refreshed window))
  new-size)

(defmethod (setf fb:location) (location (window window))
  (mezzano.gui.compositor:move-window window (car location) (cdr location)))

(defmethod (setf fb:title) (title (window window))
  (setf (mezzano.gui.widgets:frame-title (frame window)) title)
  (setf (fb-int:title window) title))

(defmethod (setf fb:visible-p) (state (window window))
  ;; TODO: implement visible-p
  )

(defmethod (setf fb:maximized-p) (state (window window))
  state)

(defmethod (setf fb:iconified-p) (state (window window))
  state)

(defmethod (setf fb:minimum-size) (value (window window))
  (setf (car (fb:minimum-size window)) (max 0 (car value)))
  (setf (cdr (fb:minimum-size window)) (max 0 (cdr value))))

(defmethod (setf fb:maximum-size) (value (window window))
  (setf (car (fb:maximum-size window)) (car value))
  (setf (cdr (fb:maximum-size window)) (cdr value)))

(defmethod (setf fb:focused-p) (value (window window))
  (when value
    (mezzano.gui.compositor::activate-window window))
  value)

(defmethod (setf fb:borderless-p) (value (window window))
  (unless (eq value (fb:borderless-p window))
    ;; FIXME: nudge the window position to keep the center frame position intact
    (setf (fb-int:borderless-p window) value)
    (update-buffer window (fb:width window) (fb:height window))
)
  value)

(defmethod (setf fb:always-on-top-p) (value (window window))
  value)

(defmethod (setf fb:resizable-p) (value (window window))
  (setf (mezzano.gui.widgets::resizablep (frame window)) value)
  (setf (fb-int:resizable-p window) value))

(defmethod (setf fb:floating-p) (value (window window))
  value)

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  (when (fb:fullscreen-p window)
    (setf (fb-int:fullscreen-p window) value)
    (setf (fb:size window) (original-size window))
    (setf (fb:location window) (original-location window)))
  value)

(defmethod (setf fb:fullscreen-p) ((value fb:video-mode) (window window))
  (unless (fb:fullscreen-p window)
    (setf (fb-int:fullscreen-p window) value)
    (setf (car (original-location window)) (car (fb:location window)))
    (setf (cdr (original-location window)) (cdr (fb:location window)))
    (setf (fb:location window) (cons 0 0))
    (setf (car (original-size window)) (car (fb:size window)))
    (setf (cdr (original-size window)) (cdr (fb:size window)))
    (setf (fb:size window) (fb:size value)))
  value)

(defmethod fb:clipboard ((window window)))

(defmethod (setf fb:clipboard) ((string string) (window window))
  string)

(defmethod (setf fb:icon) ((value null) (window window))
  value)

(defmethod (setf fb:icon) ((value fb:icon) (window window))
  value)

(defmethod (setf fb:cursor-icon) ((value symbol) (window window))
  ;; TODO: implement cursor-icon
  )

(defmethod (setf fb:cursor-icon) ((value fb:icon) (window window))
  ;; TODO: implement cursor-icon
  )

(defmethod (setf fb:cursor-state) (value (window window))
  (setf (fb-int:cursor-state window) value))

(defmethod key-scan-code (key (window window))
  (gethash key *codetable*))

(defmethod local-key-string (key (window window))
  (string (mezzano.gui.compositor::convert-scancode-to-key
           mezzano.gui.compositor::*current-keymap*
           key ())))

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (multiple-value-bind (left right top bottom) (if (or (fb:fullscreen-p window)
                                                       (fb:borderless-p window))
                                                   (values 0 0 0 0)
                                                   (mezzano.gui.widgets:frame-size (frame window)))
    ;; We have to re-encode to copy into the framebuffer. Very sad.
    ;; However, we have to ignore the frame borders unless we're borderless
    ;; anyway so copying is kinda mandatory. OH WELL. At least let's try to
    ;; do it semi-intelligently.
    (loop with src = (fb:buffer window)
          with dst = (mezzano.gui.compositor:window-buffer window)
          with srow-gap of-type (unsigned-byte 16) = (* 4 (- (fb:width window) w))
          with drow-gap of-type (unsigned-byte 16) = (- w (mezzano.gui.compositor:width window))
          with si of-type (unsigned-byte 16) = (+ (* 4 x) (* y srow-gap))
          with di of-type (unsigned-byte 16) = (+ (+ x left) (* (+ y top) drow-gap))
          for yi of-type (unsigned-byte 16) from y below (+ y h)
          do (loop for xi of-type (unsigned-byte 16) from x below (+ x w)
                   for px = (aref src (+ si 0))
                   do (setf (ldb (byte 8  8) px) (aref src (+ si 1)))
                      (setf (ldb (byte 8 16) px) (aref src (+ si 2)))
                      (setf (ldb (byte 8 24) px) (aref src (+ si 3)))
                      (setf (aref dst di) px)
                      (incf si 4)
                      (incf di 1))
             (incf si srow-gap)
             (incf di drow-gap))
    (mezzano.gui.compositor:damage-window window (+ x left) (+ y top) w h)))

(defmethod fb:process-events ((window window) &key timeout)
  (let ((fifo (mezzano.gui.compositor::mailbox window)))
    (labels ((poll-events ()
               (loop for event = (mezzano.supervisor:fifo-pop fifo nil)
                     while event do (dispatch-event window evt)))
             (wait (timeout)
               (dolist (obj (mezzano.sync:wait-for-objects-with-timeout
                             timeout
                             (list* fifo (timers window))))
                 (if (eq obj fifo)
                     (poll-events)
                     (fb:timer-triggered window obj)))))
      (etypecase timeout
        (real (wait (float timeout 0f0)))
        ((eql T) (loop until (or (not (fb:valid-p window)) (fb:close-requested-p window))
                       do (wait 1.0)))
        (null
         (poll-events)
         (dolist (timer (timers window))
           (when (mezzano.supervisor:timer-expired-p timer)
             (fb:timer-triggered window timer))))))))

(defmethod fb:request-attention ((window window)))

(defmethod fb:set-timer ((window window) delay &key repeat)
  ;; FIXME: timer repeats
  (let ((timer (mezzano.supervisor:make-timer :relative delay)))
    (push timer (timers window))
    timer))

(defmethod fb:cancel-timer ((window window) timer)
  (mezzano.supervisor:timer-disarm timer)
  (setf (timers window) (remove timer (timers window)))
  NIL)

(defmethod fb:display ((window window))
  *default-display*)

(defclass display (fb:display)
  ())

(defstruct (video-mode (:include fb:video-mode)))

(defmethod process-event ((window window) (event mezzano.gui.compositor:window-activation-event))
  (when (not (or (fb:fullscreen-p window)
                 (fb:borderless-p window)))
    (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
    (mezzano.gui.widgets:draw-frame (frame window)))
  (fb:window-focused window (mezzano.gui.compositor:state event)))

(defmethod process-event ((window window) (event mezzano.gui.compositor:mouse-event))
  (handler-case (when (or (fb:fullscreen-p window)
                          (fb:borderless-p window)
                          (not (mezzano.gui.widgets:frame-mouse-event (frame window) event)))
                  (when (or (/= 0 (mezzano.gui.compositor:mouse-x-motion event))
                            (/= 0 (mezzano.gui.compositor:mouse-y-motion event)))
                    (fb:mouse-moved window
                                    (mezzano.gui.compositor:mouse-x-position event)
                                    (mezzano.gui.compositor:mouse-y-position event)))
                  (when (< 0 (mezzano.gui.compositor:mouse-button-change event))
                    (dotimes (i 10)
                      (when (logbitp i (mezzano.gui.compositor:mouse-button-change event))
                        (fb:mouse-button-changed window (case i
                                                          (0 :left)
                                                          (1 :right)
                                                          (2 :middle)
                                                          (T i))
                                                 (if (logbitp i (mezzano.gui.compositor:mouse-button-state event))
                                                     :press :release)
                                                 mezzano.gui.compositor::*keyboard-modifier-state*)))))
    (mezzano.gui.widgets:close-button-clicked ()
      (fb:window-closed window))))

(defmethod process-event ((window window) (event mezzano.gui.compositor:window-close-event))
  (fb:window-closed window))

(defmethod process-event ((window window) (event mezzano.gui.compositor:move-event))
  (fb:window-moved window (mezzano.gui.compositor::new-x event) (mezzano.gui.compositor::new-y event)))

(defmethod process-event ((window window) (event mezzano.gui.compositor:screen-geometry-update))
  (let ((w (mezzano.gui.compositor:width event))
        (h (mezzano.gui.compositor:height event)))
    (setf (car (fb:size *default-display*)) w)
    (setf (cdr (fb:size *default-display*)) h)
    (setf (video-mode-width (fb:video-mode *default-display*)) w)
    (setf (video-mode-height (fb:video-mode *default-display*)) h)
    (when (fb:fullscreen-p window)
      (setf (fb:size window) (cons w h)))))

(defmethod process-event ((window window) (event mezzano.gui.compositor:quit-event))
  (fb:window-closed window))

(defun adjust-size (window size)
  (flet ((frob (x min max)
           (if max
               (max min (min x max))
               (max min x))))
    (setf (car size) (frob (car size) (car (fb:minimum-size window)) (car (fb:maximum-size window))))
    (setf (cdr size) (frob (cdr size) (cdr (fb:minimum-size window)) (cdr (fb:maximum-size window))))
    size))

(defmethod process-event ((window window) (event mezzano.gui.compositor:resize-request-event))
  (when (and (fb:resizable-p window)
             (not (fb:fullscreen-p window)))
    (multiple-value-bind (left right top bottom) (if (or (fb:fullscreen-p window)
                                                         (fb:borderless-p window))
                                                     (values 0 0 0 0)
                                                     (mezzano.gui.widgets:frame-size (frame window)))
      (setf (fb:size window) (adjust-size window (cons (- (mezzano.gui.compositor:width event) left right)
                                                       (- (mezzano.gui.compositor:height event) top bottom)))))))

(defmethod process-event ((window window) (event mezzano.gui.compositor:resize-event))
  (fb:window-refreshed window))

(defmethod process-event ((window window) (event mezzano.gui.compositor:key-event))
  (fb:key-changed window
                  (translate-key (mezzano.gui.compositor:key-scancode event))
                  (if (mezzano.gui.compositor:key-releasep event) :release :press)
                  (char-code (mezzano.gui.compositor:key-scancode event))
                  (mezzano.gui.compositor:key-modifier-state event)))

;; TODO: key repeats
;; TODO: double click
