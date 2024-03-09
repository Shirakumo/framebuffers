(defpackage org.shirakumo.framebuffers.test
  (:use cl)
  (:local-nicknames
   (fb org.shirakumo.framebuffers)
   (fb-int org.shirakumo.framebuffers.int))
  (:export
   framebuffers))

(in-package org.shirakumo.framebuffers.test)

(defun gradient (win)
  (declare (optimize speed))
  (fb:do-pixels (buf i x y) win
    (setf (aref buf (+ 0 i)) (mod x 256))
    (setf (aref buf (+ 1 i)) (mod y 256))
    (setf (aref buf (+ 2 i)) 0)
    (setf (aref buf (+ 3 i)) 255)))

(defun color (win r g b a)
  (fb:do-pixels (buf i x y) win
    (setf (aref buf (+ 0 i)) r)
    (setf (aref buf (+ 1 i)) g)
    (setf (aref buf (+ 2 i)) b)
    (setf (aref buf (+ 3 i)) a)))

(defun test ()
  (fb:with-window (window :size '(800 . 600))
    (fb:window-refreshed ()
      (gradient window)
      (fb:swap-buffers window))))

(defun log-events ()
  (fb:with-window (window :size '(800 . 600))
    (T (type &rest args)
       (print (list* type args)))))
