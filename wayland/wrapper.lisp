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

(defun wayland-error (window message &rest args)
  (error 'wayland-error :window window :message (format NIL "~?" message args)))

(defun np? (var)
  (if (cffi:null-pointer-p var)
      NIL
      var))

(defmacro np! (form &optional window)
  `(let ((val ,form))
     (if (cffi:null-pointer-p val)
         (wayland-error ,window "Call to ~a returned unsuccessfully" ',(car form))
         val)))

(defun try-display (display)
  (let ((display (wl:display-connect display)))
    (if (cffi:null-pointer-p display)
        NIL
        display)))

(defmethod fb-int:init-backend ((backend (eql :wayland)))
  (unless (cffi:foreign-library-loaded-p 'wl:wayland)
    (cffi:use-foreign-library wl:wayland)
    (cffi:use-foreign-library wl:xkbcommon))
  (let ((display (or (try-display (cffi:null-pointer))
                     (try-display "wayland-0")
                     (try-display "wayland-1"))))
    (if display
        (wl:display-disconnect display)
        (error 'wayland-error :message "Failed to connect to Wayland display."))))

(defmethod fb-int:shutdown-backend ((backend (eql :wayland))))

(defmethod fb-int:open-backend ((backend (eql :wayland)) &rest args &key display &allow-other-keys)
  (remf args :display)
  (let ((display (if display
                     (try-display display)
                     (or (try-display (cffi:null-pointer))
                         (try-display "wayland-0")
                         (try-display "wayland-1")))))
    (if display
        (apply #'make-instance 'window :display display args)
        (error 'wayland-error :message "Failed to connect to Wayland display."))))

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
   (pending-size :initform (cons 0 0) :accessor pending-size)))

(defmethod initialize-instance :after ((window window) &key (title (fb-int:default-title)) (size '(800 . 600)) (visible-p T))
  (let ((display (display window)))
    ;; FIXME: default size to screen size
    (destructuring-bind (w . h) size
      (fb-int:with-cleanup (fb:close window)
        (setf (fb-int:ptr-window display) window)
        (setf (registry window) (np! (wl:display-get-registry display)))
        (wl:proxy-add-listener (display window) (display-listener (listener window)) display)
        (wl:proxy-add-listener (registry window) (registry-listener (listener window)) display)
        (wl:display-dispatch display)
        (wl:display-roundtrip display)
        (unless (compositor window)
          (wayland-error NIL "Couldn't find a compositor."))
        (setf (xkb-context window) (wl:xkb-context-new 0))
        (let ((size (* w h 4)))
          (multiple-value-bind (addr fd) (mmap:mmap :anonymous :protection '(:read :write) :mmap '(:shared) :size size)
            (setf (mmap-addr window) addr)
            (setf (mmap-fd window) fd)
            (setf (shm-pool window) (np! (wl:shm-create-pool (shm window) fd size)))))
        (setf (draw-buffer window) (np! (wl:shm-pool-create-buffer (shm-pool window) 0 w h (* w 4) 1)))
        (setf (surface window) (np! (wl:compositor-create-surface (compositor window))))
        (when (fractional-scale-manager window)
          (setf (fractional-scale window) (wl:wp-fractional-scale-manager-v1-get-fractional-scale (fractional-scale-manager window) (surface window)))
          (wl:proxy-add-listener (fractional-scale-manager window) (wp-fractional-scale-listener (listener window)) (display window)))
        (when (shell-surface window)
          (wl:shell-surface-set-title (shell-surface window) title)
          (wl:shell-surface-set-toplevel (shell-surface window)))
        (when (xdg-toplevel window)
          (wl:xdg-toplevel-set-title (xdg-toplevel window) title))
        (wl:surface-attach (surface window) (draw-buffer window) 0 0)
        (wl:surface-damage (surface window) 0 0 w h)
        (wl:surface-commit (surface window))
        (setf (car (fb:size window)) w)
        (setf (cdr (fb:size window)) h)
        (setf (fb:visible-p window) visible-p)))))

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
  (let ((old-size (* (car (fb:size window)) (cdr (fb:size window)) 4))
        (size (* w h 4))
        (addr (mmap-addr window))
        (fd (mmap-fd window)))
    (mmap::mremap addr fd old-size size)
    (wl:shm-pool-resize (shm-pool window) size)
    (wl:buffer-destroy (draw-buffer window))
    (setf (draw-buffer window) (wl:shm-pool-create-buffer (shm-pool window) 0 w h (* w 4) 1))
    (setf (car (fb:size window)) w)
    (setf (cdr (fb:size window)) h)))

(defmethod (setf fb:size) (size (window window))
  (update-buffer window (car size) (cdr size))
  size)

(defmethod (setf fb:location) (location (window window))
  ;; Can't do this.
  location)

(defmethod (setf fb:title) (title (window window))
  (when (xdg-toplevel window)
    (wl:xdg-toplevel-set-title (xdg-toplevel window) title))
  (when (shell-surface window)
    (wl:shell-surface-set-title (shell-surface window) title))
  (setf (fb-int:title window) title))

(defun create-shell-objects (window)
  (when (xdg-wm-base window)
    (setf (xdg-surface window) (wl:xdg-wm-base-get-xdg-surface (xdg-wm-base window) (surface window)))
    (wl:proxy-add-listener (xdg-surface window) (xdg-surface-listener (listener window)) (display window))
    (setf (xdg-toplevel window) (wl:xdg-surface-get-toplevel (xdg-surface window)))
    (wl:proxy-add-listener (xdg-toplevel window) (xdg-toplevel-listener (listener window)) (display window))
    (wl:xdg-toplevel-set-title (xdg-toplevel window) (fb:title window))
    (when (fb:maximized-p window)
      (wl:xdg-toplevel-set-maximized (xdg-toplevel window))))
  (when (shell-surface window)
    (wl:shell-surface-set-title (shell-surface window) (fb:title window)))
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
         (setf (fb-int:iconified-p window) NIL))
        (T
         (destroy-shell-objects window)
         (wl:surface-attach (surface window) (cffi:null-pointer) 0 0)
         (wl:surface-commit (surface window))))
  (setf (fb-int:visible-p window) state))

(defmethod (setf fb:maximized-p) (state (window window))
  (when (xdg-toplevel window)
    (if state
        (wl:xdg-toplevel-set-maximized (xdg-toplevel window))
        (wl:xdg-toplevel-unset-maximized (xdg-toplevel window))))
  (setf (fb-int:maximized-p window) state))

(defmethod (setf fb:iconified-p) (state (window window))
  (when (xdg-toplevel window)
    (if state
        (wl:xdg-toplevel-set-minimized (xdg-toplevel window))))
  (setf (fb-int:iconified-p window) state))

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

(defclass display (fb:display)
  ())

(defstruct (video-mode (:include fb:video-mode)))

(defmethod fb-int:list-displays-backend ((backend (eql :BACKEND)))
  ;; TODO: implement list-displays-backend
  )

(defmethod fb:display ((window window))
  ;; TODO: implement display
  )

(defmethod fb:video-modes ((display display))
  ;; TODO: implement video-modes
  )

(defmethod fb:video-mode ((display display))
  ;; TODO: implement video-mode
  )

(defmethod (setf fb:fullscreen-p) ((value null) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod (setf fb:fullscreen-p) ((value display) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod (setf fb:fullscreen-p) ((value video-mode) (window window))
  ;; TODO: implement fullscreen-p
  )

(defmethod fb:swap-buffers ((window window) &key (x 0) (y 0) (w (fb:width window)) (h (fb:height window)) sync)
  (let ((full-width (car (fb:size window))))
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
    (wl:xdg-activation-token-v1-commit (activation-token window)))
  window)

(defmethod fb:key-scan-code (key (window window))
  (key-code key))

(defmethod fb:local-key-string (key (window window))
  (when (<= 0 key 255)
    (let* ((keycode (+ 8 key))
           (layout (wl:xkb-state-key-get-layout (xkb-state window) keycode)))
      (unless (= layout #xffffffff)
        (cffi:with-foreign-objects ((keysyms :pointer))
          (wl:xkb-keymap-key-get-syms-by-level (xkb-keymap window) keycode layout 0 keysyms)
          (setf keysyms (cffi:mem-ref keysyms :pointer))
          (unless (cffi:null-pointer-p keysyms)
            (org.shirakumo.framebuffers.xlib::keysym-string (cffi:mem-ref keysyms :uint32))))))))

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
           (loop while (and (display window) (not (fb:close-requested-p window)))
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

(define-listener display-listener
  (error ((display :pointer) (object-id :pointer) (code :uint32) (message :string))
    (print message)
    (error 'wayland-error :window window :code code :message message))
  
  (delete-id))

(define-listener registry-listener
  (global ((registry :pointer) (id :uint32) (interface :string) (version :uint32))
    (print interface)
    (cond ((string= interface "wl_compositor")
           (setf (compositor window) (np? (wl:registry-bind registry id (cffi:get-var-pointer 'wl:compositor-interface) (min version 3)))))
          ((string= interface "wl_shm")
           (setf (shm window) (np? (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shm-interface) 1))))
          ((string= interface "wl_shell")
           (setf (shell window) (np? (wl:registry-bind registry id (cffi:get-var-pointer 'wl:shell-interface) 1)))
           (setf (shell-surface window) (np? (wl:shell-get-shell-surface (shell window) (surface window))))
           (when (shell-surface window)
             (wl:proxy-add-listener (shell-surface window) (shell-surface-listener (listener window)) (display window))))
          ((string= interface "wl_seat")
           (unless (seat window)
             (setf (seat window) (np? (wl:registry-bind registry id (cffi:get-var-pointer 'wl:seat-interface) 1)))
             (wl:proxy-add-listener (seat window) (seat-listener (listener window)) (display window))))
          ((string= interface "xdg_wm_base")
           (setf (xdg-wm-base window) (np? (wl:registry-bind registry id wl:xdg-wm-base-interface 1)))
           (wl:proxy-add-listener (xdg-wm-base window) (xdg-wm-base-listener (listener window)) (display window)))
          ((string= interface "xdg_activation_v1")
           (setf (activation-manager window) (np? (wl:registry-bind registry id wl:xdg-activation-v1-interface 1))))
          ((string= interface "zwp_idle_inhibit_manager_v1")
           (setf (idle-inhibit-manager window) (np? (wl:registry-bind registry id wl:zwp-idle-inhibit-manager-v1-interface 1))))
          ((string= interface "wp_fractional_scale_manager_v1")
           (setf (fractional-scale-manager window) (np? (wl:registry-bind registry id wl:wp-fractional-scale-manager-v1-interface 1))))))

  (global-remove))

(define-listener seat-listener
  (capabilities ((seat :pointer) (caps wl:seat-capabilities))
    (cond ((and (member :keyboard caps) (null (keyboard window)))
           (setf (keyboard window) (np? (wl:seat-get-keyboard seat)))
           (wl:proxy-add-listener (keyboard window) (keyboard-listener (listener window)) (display window)))
          ((and (null (member :keyboard caps)) (keyboard window))
           (wl:proxy-destroy (keyboard window))
           (setf (keyboard window) NIL)))

    (cond ((and (member :pointer caps) (null (pointer window)))
           (setf (pointer window) (np? (wl:seat-get-pointer seat)))
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
      (when (fb:visible-p window)
        (fb:window-refreshed window)))))

(define-listener xdg-toplevel-listener
  (configure ((xdg-toplevel :pointer) (width :int32) (height :int32) (states :pointer))
    (when (and (< 0 width) (< 0 height))
      (setf (car (pending-size window)) width)
      (setf (cdr (pending-size window)) height))
    (dotimes (i (wl:array-size states))
      (case (cffi:mem-aref (wl:array-data states) 'wl:xdg-toplevel-state i)
        (:maximized (setf (fb-int:maximized-p window) T))
        (:fullscreen))))

  (close ((xdg-toplevel :pointer))
    (fb:window-closed window))

  (configure-bounds ((xdg-toplevel :pointer) (width :int32) (height :int32)))

  (wm-capabilities ((xdg-toplevel :pointer) (capabilities :pointer))))

(define-listener wp-fractional-scale-listener
  (preferred-scale ((fractional-scale :pointer) (numerator :uint32))
    (let ((scale (/ numerator 120)))
      (fb:content-scale-changed window scale scale))))

(define-listener xdg-activation-listener
  (done ((activation-token :pointer) (token :string))
    (when (cffi:pointer-eq activation-token (activation-token window))
      (wl:xdg-activation-v1-activate (activation-manager window) token (surface window))
      (wl:xdg-activation-token-v1-destroy activation-token)
      (setf (activation-token window) NIL))))

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

;; TODO: touch events
;; TODO: pen events
