(in-package #:org.shirakumo.framebuffers.BACKEND)

(pushnew :BACKEND fb-int:*available-backends*)

(define-condition BACKEND-error (fb:framebuffer-error)
  ()
  (:report (lambda (c s) (format s ""))))

(defmethod fb-int:init-backend ((backend (eql :BACKEND)))
  ;; TODO: implement init-backend
  )

(defmethod fb-int:shutdown-backend ((backend (eql :BACKEND)))
  ;; TODO: implement shutdown-backend
  )

(defmethod fb-int:open-backend ((backend (eql :BACKEND)) &key)
  ;; TODO: implement list-displays-backend
  )

(defmethod fb-int:list-displays-backend ((backend (eql :BACKEND)))
  ;; TODO: implement list-displays-backend
  )

(defclass window (fb:window)
  ((buffer :initarg :buffer :initform NIL :reader fb:buffer :accessor buffer)))

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

(defmethod (setf fb:fullscreen-p) (value (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod fb:clipboard-string ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard-string) (string (window window))
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

(defmethod fb:clipboard-string ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard-string) (string (window window))
  ;; TODO: implement clipboard setting
  )

(defmethod fb:swap-buffers ((window window))
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
