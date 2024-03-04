(in-package #:org.shirakumo.framebuffers.xlib)

(pushnew :xlib fb:*available-backends*)

(defmethod fb:init-backend ((backend (eql :xlib)))
  )

(defmethod fb:shutdown-backend ((backend (eql :xlib)))
  )

(defmethod fb:open-backend ((backend (eql :xlib)) &key)
  )

(defclass window (fb:window)
  ())

(defmethod fb:close ((window window)))

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

(defmethod fb:swap-buffers ((window window) new-buffer))

(defmethod fb:process-events ((window window) &key timeout))

(defmethod fb:request-attention ((window window)))
