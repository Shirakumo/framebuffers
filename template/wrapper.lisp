(in-package #:org.shirakumo.framebuffers.BACKEND)

(pushnew :BACKEND fb-int:*available-backends*)

(define-condition BACKEND-error (fb:framebuffer-error)
  ()
  (:report (lambda (c s) (format s ""))))

(defmethod fb-int:init-backend ((backend (eql :BACKEND))))

(defmethod fb-int:shutdown-backend ((backend (eql :BACKEND))))

(defmethod fb-int:open-backend ((backend (eql :BACKEND)) &key))

(defclass window (fb:window)
  ())

(defmethod fb:valid-p ((window window)))

(defmethod fb:close ((window window)))

(defmethod fb:close-requested-p ((window window)))

(defmethod fb:width ((window window)))

(defmethod fb:height ((window window)))

(defmethod fb:size ((window window)))

(defmethod (setf fb:size) (size (window window)))

(defmethod fb:location ((window window)))

(defmethod (setf fb:location) (location (window window)))

(defmethod fb:title ((window window)))

(defmethod (setf fb:title) (title (window window)))

(defmethod fb:visible-p ((window window)))

(defmethod (setf fb:visible-p) (state (window window)))

(defmethod fb:maximized-p ((window window)))

(defmethod (setf fb:maximized-p) (state (window window)))

(defmethod fb:iconified-p ((window window)))

(defmethod (setf fb:iconified-p) (state (window window)))

(defmethod fb:clipboard-string ((window window)))

(defmethod (setf fb:clipboard-string) (string (window window)))

(defmethod fb:content-scale ((window window)))

(defmethod fb:buffer ((window window)))

(defmethod fb:swap-buffers ((window window)))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
