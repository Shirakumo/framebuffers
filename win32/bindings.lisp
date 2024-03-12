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

(cffi:defcfun ("AdjustWindowRect") :boolean
  (rect :pointer)
  (style :int32)
  (menu :boolean))

(cffi:defcfun ("BitBlt") :bool
  (hdc :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (hdc-src :pointer)
  (x1 :int)
  (y1 :int)
  (rop :int32))

(cffi:defcfun ("ChangeDisplaySettingsW") :long
  (dev-mode :pointer)
  (flags :int32))

(cffi:defcfun ("CreateWindowExW") :pointer
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

(cffi:defcfun ("DefWindowProcW") :size
  (window :pointer)
  (message :uint)
  (wparameter :size)
  (lparameter :size))

(cffi:defcfun ("DestroyWindow") :boolean
  (window :pointer))

(cffi:defcfun ("DispatchMessageW") :size
  (message :pointer))

(cffi:defcfun ("EnableNonClientDpiScaling") :boolean
  (window :pointer))

(cffi:defcfun ("EnumDisplaySettingsW") :boolean
  (device-name :pointer)
  (mode-num :int32)
  (dev-mode :pointer))

(cffi:defcfun ("GetDC") :pointer
  (window :pointer))

(cffi:defcfun ("GetDpiForMonitor") com:hresult
  (monitor :pointer)
  (dpi-type monitor-dpi-type)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("GetDpiForWindow") :uint
  (pointer :window))

(cffi:defcfun ("GetMessageTime") :long)

(cffi:defcfun ("GetSystemMetrics") :int
  (index :int))

(cffi:defcfun ("GetWindowLongPtrW") :ssize
  (window :pointer)
  (index :int))

(cffi:defcfun ("InvalidateRect") :boolean
  (window :pointer)
  (rect :pointer)
  (erase :boolean))

(cffi:defcfun ("PeekMessageW") :boolean
  (message :pointer)
  (window :pointer)
  (message-filter-min :uint)
  (message-filter-max :uint)
  (remove-message :uint))

(cffi:defcfun ("RegisterClassW") :int16
  (class :pointer))

(cffi:defcfun ("ReleaseDC") :int
  (window :pointer)
  (dc :pointer))

(cffi:defcfun ("SendMessageW") :ssize
  (window :pointer)
  (message :uint)
  (wparameter :size)
  (lparameter :size))

(cffi:defcfun ("SetProcessDPIAware") :boolean)

(cffi:defcfun ("SetProcessDpiAwarenessContext") :boolean
  (context :pointer))

(cffi:defcfun ("SetProcessDpiAwareness") com:hresult
  (awareness dpi-awareness))

(cffi:defcfun ("SetWindowLongPtrW") :ssize
  (window :pointer)
  (index :int)
  (new-long :ssize))

(cffi:defcfun ("SetWindowPos") :boolean
  (window :pointer)
  (insert-after :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (flags :uint))

(cffi:defcfun ("ShowWindow") :boolean
  (window :pointer)
  (cmd-show :int))

(cffi:defcfun ("StretchDIBits") :int
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

(cffi:defcfun ("TranslateMessage") :boolean
  (message :pointer))

(cffi:defcfun ("ValidateRect") :boolean
  (window :pointer)
  (rect :pointer))

