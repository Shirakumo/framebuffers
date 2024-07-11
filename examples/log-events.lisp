(in-package #:org.shirakumo.framebuffers.examples)

(defun log-events ()
  (fb:with-window (window :size '(800 . 600))
    (T (type &rest args)
      (print (list* type args)))))
