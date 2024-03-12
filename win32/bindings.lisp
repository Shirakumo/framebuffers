(in-package #:org.shirakumo.framebuffers.win32.cffi)

(cffi:define-foreign-library user32
  (T (:defaults "user32")))

(cffi:define-foreign-library shcore
  (T (:defaults "shcore")))

(cffi:defcfun ("AdjustWindowRect") com:hresult
  )
(cffi:defcfun ("BitBlt") com:hresult
  )
(cffi:defcfun ("ChangeDisplaySettings") com:hresult
  )
(cffi:defcfun ("CreateWindowEx") com:hresult
  )
(cffi:defcfun ("DefWindowProc") com:hresult
  )
(cffi:defcfun ("DestroyWindow") com:hresult
  )
(cffi:defcfun ("DispatchMessage") com:hresult
  )
(cffi:defcfun ("EnableNonClientDpiScaling") com:hresult
  )
(cffi:defcfun ("EnumDisplaySettings") com:hresult
  )
(cffi:defcfun ("GetDC") com:hresult
  )
(cffi:defcfun ("GetDpiForMonitor") com:hresult
  )
(cffi:defcfun ("GetDpiForWindow") com:hresult
  )
(cffi:defcfun ("GetMessageTime") com:hresult
  )
(cffi:defcfun ("GetSystemMetrics") com:hresult
  )
(cffi:defcfun ("GetWindowLongPtr") com:hresult
  )
(cffi:defcfun ("InvalidateRect") com:hresult
  )
(cffi:defcfun ("PeekMessageW") com:hresult
  )
(cffi:defcfun ("RegisterClass") com:hresult
  )
(cffi:defcfun ("ReleaseDC") com:hresult
  )
(cffi:defcfun ("SendMessage") com:hresult
  )
(cffi:defcfun ("SetProcessDPIAware") com:hresult
  )
(cffi:defcfun ("SetProcessDpiAwarenessContext") com:hresult
  )
(cffi:defcfun ("SetWindowLongPtr") com:hresult
  )
(cffi:defcfun ("SetWindowPos") com:hresult
  )
(cffi:defcfun ("ShowWindow") com:hresult
  )
(cffi:defcfun ("StretchDIBits") com:hresult
  )
(cffi:defcfun ("TranslateMessage") com:hresult
  )
(cffi:defcfun ("ValidateRect") com:hresult
  )
