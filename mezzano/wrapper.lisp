(in-package #:org.shirakumo.framebuffers.mezzano)

(pushnew :mezzano fb-int:*available-backends*)

(define-condition mezzano-error (fb:framebuffer-error)
  ()
  (:report (lambda (c s) (format s ""))))

(defmethod fb-int:init-backend ((backend (eql :mezzano)))
  ;; TODO: implement init-backend
  )

(defmethod fb-int:shutdown-backend ((backend (eql :mezzano)))
  ;; TODO: implement shutdown-backend
  )

(defmethod fb-int:open-backend ((backend (eql :mezzano)) &rest args &key &allow-other-keys)
  (apply #'make-instance 'window :fifo (mezzano.supervisor:make-fifo 50) args))

(defmethod fb-int:list-displays-backend ((backend (eql :mezzano)))
  ;; TODO: implement list-displays-backend
  )

(defclass window (fb:window mezzano.gui.compositor:window)
  ((buffer :initarg :buffer :initform NIL :reader fb:buffer :accessor buffer)
   (frame :initform NIL :accessor frame)))

(defmethod initialize-instance :after ((window window) &key title)
  (setf (frame window) (make-instance 'mezzano.gui.widgets:frame
                                      :framebuffer (mezzano.gui.compositor:window-buffer window)
                                      :title (or title (fb-int:default-title))
                                      :close-button-p T
                                      :resizablep (fb:resizable-p window)
                                      :damage-function (mezzano.gui.widgets:default-damage-function window)
                                      :set-cursor-function (mezzano.gui.widgets:default-cursor-function window))))

(defun update-buffer (window w h)
  (let ((new-framebuffer (mezzano.gui:make-surface w h)))
    (mezzano.gui.widgets:resize-frame (frame window) new-framebuffer)
    (mezzano.gui.compositor:resize-window (window window) new-framebuffer))
  (fb-int:resize-buffer w h (buffer window) (fb:width window) (fb:height window))
  (setf (car (fb-int:size window)) w)
  (setf (cdr (fb-int:size window)) h))

(defmethod fb:valid-p ((window window))
  ;; TODO: implement valid-p
  )

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

(defmethod (setf fb:clipboard) ((string string) (window window))
  ;; TODO: implement clipboard setting
  )

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
  ;; We have to re-encode to copy into the framebuffer. Very sad.
  (loop with row-gap = (- (fb:width window) w)
        with src = (fb:buffer window)
        with dst = (mezzano.gui.compositor:window-buffer window)
        with si = 0
        with di = 0
        for yi of-type (unsigned-byte 16) from y below (+ y h)
        do (loop for xi of-type (unsigned-byte 16) from x below (+ x w)
                 for px = (aref src (+ si 0))
                 do (setf (ldb (byte 8  8) px) (aref src (+ si 1)))
                    (setf (ldb (byte 8 16) px) (aref src (+ si 2)))
                    (setf (ldb (byte 8 24) px) (aref src (+ si 3)))
                    (setf (aref dst di) px)
                    (incf si 4)
                    (incf di 1))
           (incf si (* 4 row-gap))
           (incf di row-gap))
  (mezzano.gui.compositor:damage-window window x y w h))

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


(defmethod process-event ((window window) (event mezzano.gui.compositor:window-activation-event))
  (fb:window-focused window (mezzano.gui.compositor:state event)))

(defmethod process-event ((window window) (event mezzano.gui.compositor:mouse-event))
  (handler-case (mezzano.gui.widgets:frame-mouse-event (frame window) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (fb:window-closed window))))

(defmethod process-event ((window window) (event mezzano.gui.compositor:window-close-event))
  (fb:window-closed window))

(defmethod process-event ((window window) (event mezzano.gui.compositor:quit-event))
  (fb:window-closed window))

(defun adjust-size ((window window) size)
  (flet ((frob (x min max)
           (if max
               (max min (min x max))
               (max min x))))
    (setf (car size) (frob (car size) (car (fb:minimum-size window)) (car (fb:maximum-size window))))
    (setf (cdr size) (frob (cdr size) (cdr (fb:minimum-size window)) (cdr (fb:maximum-size window))))
    size))

(defmethod process-event ((window window) (event mezzano.gui.compositor:resize-request-event))
  (let ((new-size (adjust-size window (cons (mezzano.gui.compositor:width event)
                                            (mezzano.gui.compositor:height event)))))
    (unless (equal new-size (fb:size window))
      (update-buffer window (car new-size) (cdr new-size))
      (fb:window-resized window (car new-size) (cdr new-size))
      (fb:window-refreshed window))))

(defmethod process-event ((window window) (event mezzano.gui.compositor:resize-event))
  (fb:window-refreshed window))

(defmethod process-event ((window window) (event mezzano.gui.compositor:key-event))
  (fb:key-changed window
                  (mezzano.gui.compositor:key-key event)
                  (if (mezzano.gui.compositor:key-releasep event)
                      :release :press)
                  NIL
                  NIL))
