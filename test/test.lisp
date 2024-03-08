(defpackage org.shirakumo.framebuffers.test
  (:use cl)
  (:local-nicknames
   (fb org.shirakumo.framebuffers)
   (fb-int org.shirakumo.framebuffers.int))
  (:export
   framebuffers))

(in-package org.shirakumo.framebuffers.test)

(defmacro do-pixels ((i x y buf w h &optional (channels 4)) &body body)
  `(let ((,i 0))
     (declare (type (simple-array (unsigned-byte 8) (*)) ,buf))
     (dotimes (,y ,h ,buf)
       (dotimes (,x ,w)
         (progn ,@body)
         (incf ,i ,channels)))))

(defun gradient (buf w h)
  (do-pixels (i x y buf w h)
    (setf (aref buf (+ 0 i)) (mod x 256))
    (setf (aref buf (+ 1 i)) (mod y 256))
    (setf (aref buf (+ 2 i)) (mod (* x y) 256))
    (setf (aref buf (+ 3 i)) 255)))

(defun color (buf w h r g b a)
  (do-pixels (i x y buf w h)
    (setf (aref buf (+ 0 i)) r)
    (setf (aref buf (+ 1 i)) g)
    (setf (aref buf (+ 2 i)) b)
    (setf (aref buf (+ 3 i)) a)))

(defun test ()
  (fb:with-window (window :size '(800 . 600))
    (fb:window-resized (w h)
                       (gradient (fb:buffer window) w h)
                       (fb:swap-buffers window))))

(defun test/2 ()
  (fb:with-window (window :size '(800 . 600))
    (T (type &rest args)
       (print (list type args)))))
