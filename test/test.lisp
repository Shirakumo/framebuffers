(defpackage #:org.shirakumo.framebuffers.test
  (:use #:cl)
  (:local-nicknames
   (#:fb #:org.shirakumo.framebuffers)
   (#:fb-int #:org.shirakumo.framebuffers.int))
  (:export
   #:gradient
   #:log-events))

(in-package #:org.shirakumo.framebuffers.test)

(defun gradient ()
  (fb:with-window (window :size '(800 . 600))
    (fb:window-refreshed ()
      (fb:do-pixels (buf i x y) window
        (setf (aref buf (+ 0 i)) (mod x 256))
        (setf (aref buf (+ 1 i)) (mod y 256))
        (setf (aref buf (+ 2 i)) 0)
        (setf (aref buf (+ 3 i)) 255))
      (fb:swap-buffers window))))

(defun log-events ()
  (fb:with-window (window :size '(800 . 600))
    (T (type &rest args)
      (print (list* type args)))))
