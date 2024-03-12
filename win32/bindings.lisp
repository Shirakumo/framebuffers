(in-package #:org.shirakumo.framebuffers.win32.cffi)

(cffi:define-foreign-library user32
  (T (:defaults "user32")))

(cffi:define-foreign-library shcore
  (T (:defaults "shcore")))

(cffi:defcenum monitor-dpi-type
  (:effective-dpi 0)
  (:angular-dpi 1)
  (:raw-dpi 2))

(cffi:defcenum dpi-awareness
  (:unaware 0)
  (:system-dpi-aware 1)
  (:per-monitor-dpi-aware 2))

(cffi:defcstruct (rect :conc-name rect-)
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(cffi:defcfun (adjust-window-rect "AdjustWindowRect") :boolean
  (rect :pointer)
  (style :int32)
  (menu :boolean))

(cffi:defcfun (bit-blt "BitBlt") :bool
  (hdc :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (hdc-src :pointer)
  (x1 :int)
  (y1 :int)
  (rop :int32))

(cffi:defcfun (change-display-settings "ChangeDisplaySettingsW") :long
  (dev-mode :pointer)
  (flags :int32))

(cffi:defcfun (create-window "CreateWindowExW") :pointer
  (ex-style :int32)
  (class-name :pointer)
  (window-name :pointer)
  (style :int32)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (parent :pointer)
  (menu :pointer)
  (instance :pointer)
  (param :pointer))

(cffi:defcfun (def-window-proc "DefWindowProcW") :size
  (window :pointer)
  (message :uint)
  (wparameter :size)
  (lparameter :size))

(cffi:defcfun (destroy-window "DestroyWindow") :boolean
  (window :pointer))

(cffi:defcfun (dispatch-message "DispatchMessageW") :size
  (message :pointer))

(cffi:defcfun (enable-non-client-dpi-scaling "EnableNonClientDpiScaling") :boolean
  (window :pointer))

(cffi:defcfun (enum-display-settings "EnumDisplaySettingsW") :boolean
  (device-name :pointer)
  (mode-num :int32)
  (dev-mode :pointer))

(cffi:defcfun (get-dc "GetDC") :pointer
  (window :pointer))

(cffi:defcfun (get-dpi-for-monitor "GetDpiForMonitor") com:hresult
  (monitor :pointer)
  (dpi-type monitor-dpi-type)
  (x :pointer)
  (y :pointer))

(cffi:defcfun (get-dpi-for-window "GetDpiForWindow") :uint
  (pointer :window))

(cffi:defcfun (get-message-time "GetMessageTime") :long)

(cffi:defcfun (get-system-metrics "GetSystemMetrics") :int
  (index :int))

(cffi:defcfun (get-window "GetWindowLongPtrW") :ssize
  (window :pointer)
  (index :int))

(cffi:defcfun (invalidate-rect "InvalidateRect") :boolean
  (window :pointer)
  (rect :pointer)
  (erase :boolean))

(cffi:defcfun (peek-message "PeekMessageW") :boolean
  (message :pointer)
  (window :pointer)
  (message-filter-min :uint)
  (message-filter-max :uint)
  (remove-message :uint))

(cffi:defcfun (register-class "RegisterClassW") :int16
  (class :pointer))

(cffi:defcfun (release-dc "ReleaseDC") :int
  (window :pointer)
  (dc :pointer))

(cffi:defcfun (send-message "SendMessageW") :ssize
  (window :pointer)
  (message :uint)
  (wparameter :size)
  (lparameter :size))

(cffi:defcfun (set-process-dpi-aware "SetProcessDPIAware") :boolean)

(cffi:defcfun (set-process-dpi-awareness-context "SetProcessDpiAwarenessContext") :boolean
  (context :pointer))

(cffi:defcfun (set-process-dpi-awareness "SetProcessDpiAwareness") com:hresult
  (awareness dpi-awareness))

(cffi:defcfun (set-window "SetWindowLongPtrW") :ssize
  (window :pointer)
  (index :int)
  (new-long :ssize))

(cffi:defcfun (set-window-pos "SetWindowPos") :boolean
  (window :pointer)
  (insert-after :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (flags :uint))

(cffi:defcfun (show-window "ShowWindow") :boolean
  (window :pointer)
  (cmd-show :int))

(cffi:defcfun (stretch-di-bits"StretchDIBits") :int
  (window :pointer)
  (xdest :int)
  (ydest :int)
  (wdest :int)
  (hdest :int)
  (xsrc :int)
  (ysrc :int)
  (wsrc :int)
  (hsrc :int)
  (rop :int32))

(cffi:defcfun (translate-message "TranslateMessage") :boolean
  (message :pointer))

(cffi:defcfun (validate-rect "ValidateRect") :boolean
  (window :pointer)
  (rect :pointer))

