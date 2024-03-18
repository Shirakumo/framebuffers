(in-package #:org.shirakumo.framebuffers.int)

(cffi:defcstruct (itimer :conc-name itimer-)
  (interval-sec :size)
  (interval-nsec :size)
  (value-sec :size)
  (value-nsec :size))

(cffi:defcenum clockid
  (:realtime 0)
  (:monotonic 1)
  (:boottime 7)
  (:realtime-alarm 8)
  (:boottime-alarm 9))

(cffi:defbitfield timerfd-flag
  (:cloexec  #o2000000)
  (:nonblock #o0004000))

(cffi:defcfun (timerfd-create "timerfd_create") :int
  (clockid clockid)
  (flags timerfd-flag))

(cffi:defcfun (timerfd-set-time "timerfd_settime") :int
  (fd :int)
  (flags :)
  (new :pointer)
  (old :pointer))

(cffi:defcfun (timerfd-get-time "timerfd_gettime") :int
  (fd :int)
  (value :pointer))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd :int)
  (events :short)
  (revents :short))

(cffi:defcfun (%poll "poll") :int
  (fds :pointer)
  (count :int)
  (timeout :int))

(defmacro with-poll (fds &body body)
  (let ((gfds (gensym "FDS")) (count (gensym "COUNT")) (pollfds (gensym "POLLFDS")))
    `(let* ((,gfds ,fds)
            (,count (length ,gfds)))
       (cffi:with-foreign-objects ((,pollfds '(:struct pollfd) ,count))
         (loop for i from 0
               for fd in ,gfds
               for pollfd = (cffi:mem-aptr ,pollfds '(:struct pollfd) i)
               do (setf (pollfd-fd pollfd) fd)
                  (setf (pollfd-events pollfd) 1))
         (flet ((poll (timeout)
                  (loop for i from 0 below ,count
                        for pollfd = (cffi:mem-aptr ,pollfds '(:struct pollfd) i)
                        do (setf (pollfd-revents pollfd) 0))
                  (when (< 0 (%poll ,pollfds ,count timeout))
                    (loop for i from 0 below ,count
                          for pollfd = (cffi:mem-aptr ,pollfds '(:struct pollfd) i)
                          when (< 0 (pollfd-revents pollfd))
                          collect (pollfd-fd pollfd)))))
           ,@body)))))

(defclass linux-window (window)
  ((timers :initform () :accessor timers)))

(defmethod fb:set-timer ((window linux-window) delay &key repeat)
  (let ((fd (timerfd-create :realtime ())))
    (cffi:with-foreign-objects ((itimer '(:struct itimer)))
      (multiple-value-bind (secs nsecs) (truncate delay)
        (setf nsecs (truncate (* nsecs 1000000000)))
        (setf (itimer-interval-sec itimer) (if repeat secs 0))
        (setf (itimer-interval-nsec itimer) (if repeat nsecs 0))
        (setf (itimer-value-sec itimer) secs)
        (setf (itimer-value-nsec itimer) nsecs))
      (timerfd-set-time fd itimer (cffi:null-pointer)))
    (push fd (timers window))
    fd))

(defmethod fb:cancel-timer ((window linux-window) timer)
  (cffi:foreign-funcall "close" :int timer))
