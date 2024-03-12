(in-package #:org.shirakumo.framebuffers.wayland)

(defun getenv (name)
  (let ((res (cffi:foreign-funcall "getenv" :string name :string)))
    (when (and res (string/= "" res))
      res)))

(pushnew :wayland fb-int:*available-backends*)

(define-condition wayland-error (fb:framebuffer-error)
  ((code :initarg :code :initform NIL :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "A Wayland call failed~@[ (~a)~]~@[:~%  ~a~]"
                                 (code c) (message c)))))

(defmethod fb-int:init-backend ((backend (eql :wayland)))
  (unless (cffi:foreign-library-loaded-p 'wl:wayland)
    (cffi:use-foreign-library wl:wayland)
    (cffi:use-foreign-library wl:xkbcommon)
    (let ((display (wl:display-connect (cffi:null-pointer))))
      (if (cffi:null-pointer-p display)
          (error 'wayland-error :message "Failed to connect to Wayland display.")
          (wl:display-disconnect display)))))

(defmethod fb-int:shutdown-backend ((backend (eql :wayland))))

(defmethod fb-int:open-backend ((backend (eql :wayland)) &key)
  (let ((display (wl:display-connect (cffi:null-pointer))))
    (if (cffi:null-pointer-p display)
        (error 'wayland-error :message "Failed to connect to Wayland display.")
        (make-instance 'window :display display))))

(defclass window (fb:window)
  ((display :initarg :display :initform NIL :accessor display)
   (listener :initform (make-listener) :accessor listener)
   (shell-surface :initform NIL :accessor shell-surface)
   (shell :initform NIL :accessor shell)
   (surface :initform NIL :accessor surface)
   (draw-buffer :initform NIL :accessor draw-buffer)
   (shm-pool :initform NIL :accessor shm-pool)
   (shm :initform NIL :accessor shm)
   (compositor :initform NIL :accessor compositor)
   (keyboard :initform NIL :accessor keyboard)
   (pointer :initform NIL :accessor pointer)
   (seat :initform NIL :accessor seat)
   (registry :initform NIL :accessor registry)
   (xdg-wm-base :initform NIL :accessor xdg-wm-base)
   (xdg-decoration :initform NIL :accessor xdg-decoration)
   (xdg-toplevel :initform NIL :accessor xdg-toplevel)
   (xdg-surface :initform NIL :accessor xdg-surface)
   (xkb-context :initform NIL :accessor xkb-context)
   (xkb-compose-state :initform NIL :accessor xkb-compose-state)
   (xkb-state :initform NIL :accessor xkb-state)
   (xkb-keymap :initform NIL :accessor xkb-keymap)
   (activation-manager :initform NIL :accessor activation-manager)
   (activation-token :initform NIL :accessor activation-token)
   (idle-inhibit-manager :initform NIL :accessor idle-inhibit-manager)
   (fractional-scale-manager :initform NIL :accessor fractional-scale-manager)
   (fractional-scale :initform NIL :accessor fractional-scale)
   (mmap-fd :initform NIL :accessor mmap-fd)
   (mmap-addr :initform NIL :accessor mmap-addr)

   (moddefs :initform (copy-tree *moddef-table*) :accessor moddefs)
   (modifiers :initform () :accessor modifiers)
   (buffer :initform NIL :reader fb:buffer :accessor buffer)
   (content-scale :initform (cons 1 1) :reader fb:content-scale :accessor content-scale)
   (close-requested-p :initform NIL :reader fb:close-requested-p :accessor close-requested-p)
   (size :initform (cons 1 1) :reader fb:size :accessor size)
   (pending-size :initform (cons 0 0) :accessor pending-size)
   (location :initform (cons 0 0) :reader fb:location :accessor location)
   (title :initform NIL :reader fb:title :accessor title)
   (visible-p :initform T :reader fb:visible-p :accessor visible-p)
   (maximized-p :initform NIL :reader fb:maximized-p :accessor maximized-p)
   (iconified-p :initform NIL :reader fb:iconified-p :accessor iconified-p)))

(defmethod initialize-instance :after ((window window) &key (title (fb-int:default-title)) (size '(800 . 600)) (visible-p T))
  (let ((display (display window)))
    ;; FIXME: default size to screen size
    (destructuring-bind (w . h) size
      (fb-int:with-cleanup (fb:close window)
        (setf (fb-int:ptr-window display) window)
        (setf (registry window) (wl:display-get-registry display))
        (wl:proxy-add-listener (display window) (display-listener (listener window)) display)
        (wl:proxy-add-listener (registry window) (registry-listener (listener window)) display)
        (when (= -1 (wl:display-dispatch display))
          (error 'wayland-error :message "Failed to dispatch display."))
        (when (= -1 (wl:display-roundtrip display))
          (error 'wayland-error :message "Failed to roundtrip display."))
        (when (= -1 (compositor window))
          (error 'wayland-error :message "Couldn't find a compositor."))
        (setf (xkb-context window) (wl:xkb-context-new 0))
        (let ((size (* w h 4)))
          (multiple-value-bind (addr fd) (mmap:mmap :anonymous :protection '(:read :write) :mmap '(:shared) :size size)
            (setf (mmap-addr window) addr)
            (setf (mmap-fd window) addr)
            (setf (shm-pool window) (wl:shm-create-pool (shm window) fd size))))
        (setf (draw-buffer window) (wl:shm-pool-create-buffer (shm-pool window) 0 w h (* w 4) 1))
        (setf (surface window) (wl:compositor-create-surface (compositor window)))
        (when (fractional-scale-manager window)
          (setf (fractional-scale window) (wl:wp-fractional-scale-manager-v1-get-fractional-scale (fractional-scale-manager window) (surface window)))
          (wl:proxy-add-listener (fractional-scale-manager window) (wp-fractional-scale-listener (listener window)) (display window)))
        (setf (shell-surface window) (wl:shell-get-shell-surface (shell window) (surface window)))
        (when (cffi:null-pointer-p (shell-surface window))
          (setf (shell-surface window) NIL))
        (when (shell-surface window)
          (wl:proxy-add-listener (shell-surface window) (shell-surface-listener (listener window)) display)
          (wl:shell-surface-set-title (shell-surface window) title)
          (wl:shell-surface-set-toplevel (shell-surface window)))
        (wl:surface-attach (surface window) (draw-buffer window) 0 0)
        (wl:surface-damage (surface window) 0 0 w h)
        (wl:surface-commit (surface window))
        (setf (car (size window)) w)
        (setf (cdr (size window)) h)
        (setf (visible-p window) visible-p)))))

(defmethod fb:valid-p ((window window))
  (not (null (display window))))

(defmethod fb:close ((window window))
  (macrolet ((clean (accessor free)
               `(let ((,accessor (,accessor window)))
                  (when ,accessor
                    (,free ,accessor)
                    (setf (,accessor window) NIL)))))
    (dolist (slot '(shell-surface shell surface shm-pool shm compositor keyboard pointer registry xdg-wm-base xdg-decoration xdg-toplevel xdg-surface))
      (when (slot-value window slot)
        (wl:proxy-destroy (slot-value window slot))
        (setf (slot-value window slot) NIL)))
    (clean xkb-compose-state wl:xkb-compose-state-unref)
    (clean xkb-keymap wl:xkb-keymap-unref)
    (clean xkb-state wl:xkb-state-unref)
    (clean xkb-context wl:xkb-context-unref)
    (clean activation-token wl:xdg-activation-token-v1-destroy)
    (clean activation-manager wl:xdg-activation-v1-destroy)
    (clean fractional-scale wl:wp-fractional-scale-v1-destroy)
    (clean fractional-scale-manager wl:wp-fractional-scale-manager-v1-destroy)
    (clean idle-inhibit-manager wl:zwp-idle-inhibit-manager-v1-destroy)
    (clean listener cffi:foreign-free)
    (clean draw-buffer wl:buffer-destroy)
    (clean buffer static-vectors:free-static-vector)
    (when (mmap-addr window)
      (mmap:munmap (mmap-addr window) (mmap-fd window) (* (fb:width window) (fb:height window) 4))
      (setf (mmap-addr window) NIL)
      (setf (mmap-fd window) NIL))
    (when (display window)
      (setf (fb-int:ptr-window (display window)) NIL)
      (wl:display-disconnect (display window))
      (setf (display window) NIL))))

(defun update-buffer (window w h)
  (let ((old-size (* (car (size window)) (cdr (size window)) 4))
        (size (* w h 4))
        (addr (mmap-addr window))
        (fd (mmap-fd window)))
    (mmap::mremap addr fd old-size size)
    (wl:shm-pool-resize (shm-pool window) size)
    (wl:buffer-destroy (draw-buffer window))
    (setf (draw-buffer window) (wl:shm-pool-create-buffer (shm-pool window) 0 w h (* w 4) 1))
    (setf (car (size window)) w)
    (setf (cdr (size window)) h)))

(defmethod fb:width ((window window))
  (car (fb:size window)))

(defmethod fb:height ((window window))
  (cdr (fb:size window)))

(defmethod (setf fb:size) (size (window window))
  (update-buffer window (car size) (cdr size))
  size)

(defmethod (setf fb:location) (location (window window))
  ;; Can't do this.
  location)

(defmethod (setf fb:title) (title (window window))
  (when (xdg-toplevel window)
    (wl:xdg-toplevel-set-title (xdg-toplevel window) (title window)))
  (when (shell-surface window)
    (wl:shell-surface-set-title (shell-surface window) title))
  (setf (title window) title))

(defun create-shell-objects (window)
  (when (xdg-wm-base window)
    (setf (xdg-surface window) (wl:xdg-wm-base-get-xdg-surface (xdg-wm-base window) (surface window)))
    (wl:proxy-add-listener (xdg-surface window) (xdg-surface-listener (listener window)) (display window))
    (setf (xdg-toplevel window) (wl:xdg-surface-get-toplevel (xdg-surface window)))
    (wl:proxy-add-listener (xdg-toplevel window) (xdg-toplevel-listener (listener window)) (display window))
    (wl:xdg-toplevel-set-title (xdg-toplevel window) (title window))
    (when (maximized-p window)
      (wl:xdg-toplevel-set-maximized (xdg-toplevel window))))
  (wl:surface-commit (surface window))
  (wl:display-roundtrip (display window)))

(defun destroy-shell-objects (window)
  (when (xdg-decoration window)
    (wl:zxdg-toplevel-decoration-v1-destroy (xdg-decoration window))
    (setf (xdg-decoration window) NIL))
  (when (xdg-toplevel window)
    (wl:proxy-destroy (xdg-toplevel window))
    (setf (xdg-toplevel window) NIL))
  (when (xdg-surface window)
    (wl:proxy-destroy (xdg-surface window))
    (setf (xdg-surface window) NIL)))

(defmethod (setf fb:visible-p) (state (window window))
  (cond (state
         (create-shell-objects window)
         (setf (iconified-p window) NIL))
        (T
         (destroy-shell-objects window)
         (wl:surface-attach (surface window) (cffi:null-pointer) 0 0)
         (wl:surface-commit (surface window))))
  (setf (visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window))
  (when (xdg-toplevel window)
    (if state
        (wl:xdg-toplevel-set-maximized (xdg-toplevel window))
        (wl:xdg-toplevel-unset-maximized (xdg-toplevel window))))
  (setf (maximized-p window) state))

(defmethod (setf fb:iconified-p) (state (window window))
  (when (xdg-toplevel window)
    (if state
        (wl:xdg-toplevel-set-minimized (xdg-toplevel window))))
  (setf (iconified-p window) state))

(defmethod fb:clipboard-string ((window window))
  ;; TODO: implement clipboard fetching
  )

(defmethod (setf fb:clipboard-string) (string (window window))
  ;; TODO: implement clipboard setting
  )

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (car (size window))) (h (car (size window))) sync)
  (let ((full-width (car (size window))))
    ;; FIXME: Could probably improve how much we have to copy, here.
    (cffi:foreign-funcall "memcpy"
                          :pointer (mmap-addr window)
                          :pointer (static-vectors:static-vector-pointer (buffer window) :offset (* 4 y full-width))
                          :size (* 4 h full-width)))
  (wl:surface-attach (surface window) (draw-buffer window) 0 0)
  (wl:surface-damage (surface window) x y w h)
  (cond (sync
         (let ((cb (wl:surface-frame (surface window))))
           (unwind-protect
                (cffi:with-foreign-objects ((done :char))
                  (setf (cffi:mem-ref done :char) 0)
                  (wl:proxy-add-listener cb (frame-listener (listener window)) done)
                  (wl:surface-commit (surface window))
                  (loop while (and (= 0 (cffi:mem-ref done :char)) (display window))
                        do (when (or (= -1 (wl:display-dispatch (display window)))
                                     (= -1 (wl:display-roundtrip (display window))))
                             (error 'wayland-error :message "Lost connection to display"))))
             (wl:proxy-destroy cb))))
        (T
         (wl:surface-commit (surface window)))))

(defmethod fb:request-attention ((window window))
  (when (activation-manager window)
    (when (activation-token window)
      (wl:xdg-activation-token-v1-destroy (activation-token window)))
    (setf (activation-token window) (wl:xdg-activation-v1-get-activation-token (activation-manager window)))
    (wl:proxy-add-listener (activation-token window) (xdg-activation-listener (listener window)) (display window))
    (wl:xdg-activation-token-v1-commit (activation-token window))))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd :int)
  (events :short)
  (revents :short))

(defmethod fb:process-events ((window window) &key timeout)
  (let ((display (display window)))
    (cffi:with-foreign-objects ((fd '(:struct pollfd)))
      (setf (pollfd-fd fd) (wl:display-get-fd display))
      (setf (pollfd-events fd) 1)
      (setf (pollfd-revents fd) 0)
      (flet ((poll (millis)
               (let ((res (cffi:foreign-funcall "poll" :pointer fd :int 1 :int millis :int)))
                 (cond ((< 0 res)
                        (wl:display-read-events display)
                        (wl:display-dispatch-pending display))
                       ((< res 0)
                        (wl:display-cancel-read display)
                        NIL)))))
        (etypecase timeout
          (null
           (wl:display-dispatch-pending display))
          (real
           (cond ((/= 0 (wl:display-prepare-read display))
                  (wl:display-dispatch-pending display))
                 (T
                  (wl:display-flush display)
                  (poll (truncate (* 1000 timeout))))))
          ((eql T)
           (loop while (display window)
                 do (loop while (/= 0 (wl:display-prepare-read display))
                          do (wl:display-dispatch-pending display))
                    (poll 1000))))))))

(defmacro define-listener (name &body callbacks)
  `(wl:define-listener ,name
     ,@(loop for (cb args . body) in callbacks
             collect (if body
                         `(,cb :void ((window :pointer) ,@args)
                               (declare (ignorable ,@(mapcar #'car args)))
                               (let ((window (fb-int:ptr-window window)))
                                 (when window
                                   ,@body)))
                         `(,cb NIL)))))

(trivial-indent:define-indentation define-listener
    (4 &rest (&whole 2 6 &body)))

(defmacro define-whole-listener (&rest listeners)
  `(progn 
     (cffi:defcstruct listener
       ,@(loop for listener in listeners
               collect `(,listener (:struct ,listener))))

     ,@(loop for listener in listeners
             collect `(defun ,listener (struct)
                        (cffi:foreign-slot-pointer struct '(:struct listener) ',listener)))

     (defun make-listener (&optional (struct (cffi:foreign-alloc '(:struct listener))))
       ,@(loop for listener in listeners
               collect `(,(intern (format NIL "~a-~a" (symbol-name 'make) (symbol-name listener))) (,listener struct)))
       struct)))

(define-listener display-listener
  (error ((display :pointer) (object-id :pointer) (code :uint32) (message :string))
    (error 'wayland-error :window window :code code :message message))
  
  (delete-id))

(define-listener registry-listener
  (global ((registry :pointer) (id :uint32) (interface :string) (version :uint32))
    (cond ((string= interface "wl_compositor")
           (setf (compositor window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:compositor-interface) 1)))
          ((string= interface "wl_shm")
           (setf (shm window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shm-interface) 1)))
          ((string= interface "wl_shell")
           (setf (shell window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shell-interface) 1)))
          ((string= interface "wl_seat")
           (setf (seat window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:seat-interface) 1))
           (wl:proxy-add-listener (seat window) (seat-listener (listener window)) (display window)))
          ((string= interface "xdg_wm_base")
           (setf (xdg-wm-base window) (wl:registry-bind registry id (cffi:get-var-pointer 'wl:xdg-wm-base-interface) 1))
           (wl:proxy-add-listener (xdg-wm-base window) (xdg-wm-base-listener (listener window)) (display window)))
          ((string= interface "xdg_activation_v1")
           (setf (activation-manager window) (wl:registry-bind registry id wl:xdg-activation-v1-interface 1)))
          ((string= interface "zwp_idle_inhibit_manager_v1")
           (setf (idle-inhibit-manager window) (wl:registry-bind registry id wl:zwp-idle-inhibit-manager-v1-interface 1)))
          ((string= interface "wp_fractional_scale_manager_v1")
           (setf (fractional-scale-manager window) (wl:registry-bind registry id wl:wp-fractional-scale-manager-v1-interface 1)))))

  (global-remove))

(define-listener seat-listener
  (capabilities ((seat :pointer) (caps wl:seat-capabilities))
    (cond ((and (member :keyboard caps) (null (keyboard window)))
           (setf (keyboard window) (wl:seat-get-keyboard seat))
           (wl:proxy-add-listener (keyboard window) (keyboard-listener (listener window)) (display window)))
          ((and (null (member :keyboard caps)) (keyboard window))
           (wl:proxy-destroy (keyboard window))
           (setf (keyboard window) NIL)))

    (cond ((and (member :pointer caps) (null (pointer window)))
           (setf (pointer window) (wl:seat-get-pointer seat))
           (wl:proxy-add-listener (pointer window) (pointer-listener (listener window)) (display window)))
          ((and (null (member :pointer caps)) (pointer window))
           (wl:proxy-destroy (pointer window))
           (setf (pointer window) NIL))))

  (name ((seat :pointer) (name :string))))

(define-listener pointer-listener
  (enter ((pointer :pointer) (serial :uint32) (surface :pointer) (sx :uint32) (sy :uint32))
    (fb:mouse-entered window T))
  
  (leave ((pointer :pointer) (serial :uint32) (surface :pointer))
    (fb:mouse-entered window NIL))
  
  (motion ((pointer :pointer) (time :uint32) (sx :uint32) (sy :uint32))
    (fb:mouse-moved window (/ sx 256) (/ sy 256)))
  
  (button ((pointer :pointer) (serial :uint32) (time :uint32) (button :uint32) (state :uint32))
    ;; TODO: double click
    (fb:mouse-button-changed
     window
     (case button
       (110 :left)
       (111 :right)
       (112 :middle)
       (T (- button 110)))
     (case state
       (0 :released)
       (1 :pressed))
     (modifiers window)))
  
  (axis ((pointer :pointer) (time :uint32) (axis :uint32) (value :uint32))
    (case axis
      (0 (fb:mouse-scrolled window 0 (- (/ value 256))))
      (1 (fb:mouse-scrolled window (- (/ value 256)) 0))))
  
  (frame ((pointer :pointer)))
  
  (axis-source ((pointer :pointer) (axis-source :uint32)))
  
  (axis-stop ((pointer :pointer) (time :uint32) (axis :uint32)))
  
  (axis-discrete ((pointer :pointer) (axis :uint32) (discrete :int32))))

(defun handle-input-text (window scancode)
  (cffi:with-foreign-objects ((keysyms :pointer)
                              (str :char 8))
    (when (= 1 (wl:xkb-state-key-get-syms (xkb-state window) (+ scancode 8) keysyms))
      (let* ((keysym (cffi:mem-aref (cffi:mem-ref keysyms :pointer) :uint32 0))
             (str (if (or (= 0 keysym)
                          (null (xkb-compose-state window))
                          (not (eql :accepted (wl:xkb-compose-state-feed (xkb-compose-state window) keysym))))
                      (org.shirakumo.framebuffers.xlib::keysym-string keysym)
                      (case (wl:xkb-compose-state-get-status (xkb-compose-state window))
                        (:composed
                         (wl:xkb-compose-state-get-utf8 (xkb-compose-state window) str 8)
                         (cffi:mem-ref str :string))
                        ((:composing :cancelled))
                        (T (org.shirakumo.framebuffers.xlib::keysym-string keysym))))))
        (when str (fb:string-entered window str))))))

(define-listener keyboard-listener
  (keymap ((keyboard :pointer) (format :uint32) (fd :int) (size :uint32))
    (mmap:with-mmap (addr fd size fd :size size :mmap '(:shared))
      (let* ((keymap (wl:xkb-keymap-new-from-string (xkb-context window) addr 1 0))
             (state (wl:xkb-state-new keymap))
             (locale (or (getenv "LC_ALL") (getenv "LC_CTYPE") (getenv "LANG") "C"))
             (compose-table (wl:xkb-compose-table-new-from-locale (xkb-context window) locale 0))
             (compose-state (wl:xkb-compose-state-new compose-table 0)))
        (setf (xkb-compose-state window) compose-state)
        (setf (xkb-state window) state)
        (setf (xkb-keymap window) keymap)
        (dolist (mod (moddefs window))
          (setf (second mod) (wl:xkb-keymap-mod-get-index keymap (third mod))))
        (wl:xkb-compose-table-unref compose-table)
        (wl:xkb-keymap-unref keymap)
        (wl:xkb-state-unref state)))
    (cffi:foreign-funcall "close" :int fd))
  
  (enter ((keyboard :pointer) (serial :uint32) (surface :pointer) (keys :pointer))
    (fb:window-focused window T))
  
  (leave ((keyboard :pointer) (serial :uint32) (surface :pointer))
    (fb:window-focused window NIL))
  
  (key ((keyboard :pointer) (serial :uint32) (time :uint32) (scancode :uint32) (state :uint32))
    (when (= state 1)
      (handle-input-text window scancode))
    (fb:key-changed window (translate-key scancode) scancode (ecase state (1 :pressed) (0 :released))
                    (modifiers window)))
  
  (modifiers ((keyboard :pointer) (serial :uint32) (mods-depressed :uint32) (mods-latched :uint32) (mods-locked :uint32) (group :uint32))
    (wl:xkb-state-update-mask (xkb-state window) mods-depressed mods-latched mods-locked 0 0 group)
    (setf (modifiers window) (loop for (mod index) in (moddefs window)
                                   when (< 0 (wl:xkb-state-mod-index-is-active (xkb-state window) index 1))
                                   collect mod)))
  
  (repeat-info ((keyboard :pointer) (rate :int32) (delay :int32))
    ;; TODO: key repeats
    ))

(define-listener shell-surface-listener
  (ping ((shell-surface :pointer) (serial :uint32))
    (wl:shell-surface-pong shell-surface serial))

  (configure ((shell-surface :pointer) (edges :uint32) (width :int32) (height :int32))
    (update-buffer window width height)
    (fb:window-resized window width height))

  (popup-done ((shell-surface :pointer))))

(wl:define-listener frame-listener
  (done :void ((var :pointer) (callback :pointer) (cookie :uint32))
    (declare (ignorable callback cookie))
    (setf (cffi:mem-ref var :char) 1)))

(define-listener xdg-wm-base-listener
  (ping ((xdg-wm-base :pointer) (serial :uint32))
    (wl:xdg-wm-base-pong xdg-wm-base serial)))

(define-listener xdg-surface-listener
  (configure ((xdg-surface :pointer) (serial :uint32))
    (wl:xdg-surface-ack-configure xdg-surface serial)
    
    (when (< 0 (car (pending-size window)))
      (update-buffer window (car (pending-size window)) (cdr (pending-size window)))
      (fb:window-resized window (car (pending-size window)) (cdr (pending-size window)))
      (setf (car (pending-size window)) 0 (cdr (pending-size window)) 0)
      (when (visible-p window)
        (fb:window-refreshed window)))))

(define-listener xdg-toplevel-listener
  (configure ((xdg-toplevel :pointer) (width :int32) (height :int32) (states :pointer))
    (when (and (< 0 width) (< 0 height))
      (setf (car (pending-size window)) width)
      (setf (cdr (pending-size window)) height))
    (dotimes (i (wl:array-size states))
      (case (cffi:mem-aref (wl:array-data states) 'wl:xdg-toplevel-state i)
        (:maximized (setf (maximized-p window) T))
        (:fullscreen))))

  (close ((xdg-toplevel :pointer))
    (setf (close-requested-p window) T)
    (fb:window-closed window))

  (configure-bounds ((xdg-toplevel :pointer) (width :int32) (height :int32)))

  (wm-capabilities ((xdg-toplevel :pointer) (capabilities :pointer))))

(define-listener wp-fractional-scale-listener
  (preferred-scale ((fractional-scale :pointer) (numerator :uint32))
    (let ((scale (/ numerator 120)))
      (setf (car (content-scale window)) scale
            (cdr (content-scale window)) scale))))

(define-listener xdg-activation-listener
  (done ((activation-token :pointer) (token :string))
    (when (cffi:pointer-eq activation-token (activation-token window))
      (wl:xdg-activation-v1-activate (activation-manager window) token (surface window))
      (wl:xdg-activation-token-v1-destroy activation-token)
      (setf (activation-token window) NIL))))

(define-whole-listener
  display-listener
  registry-listener
  seat-listener
  pointer-listener
  keyboard-listener
  shell-surface-listener
  frame-listener
  xdg-wm-base-listener
  xdg-surface-listener
  xdg-toplevel-listener
  wp-fractional-scale-listener
  xdg-activation-listener)
