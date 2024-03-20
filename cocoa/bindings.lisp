(in-package #:org.shirakumo.framebuffers.cocoa.cffi)

(cffi:defcstruct (point :conc-name point-)
  (x objc:cgfloat)
  (y objc:cgfloat))

(cffi:defcstruct (size :conc-name size-)
  (w objc:cgfloat)
  (h objc:cgfloat))

(cffi:defcstruct (rect :conc-name rect-)
  (x objc:cgfloat)
  (y objc:cgfloat)
  (w objc:cgfloat)
  (h objc:cgfloat))

nsapplication-shared-application
nsapp-set-activation-policy
nsapp-activate-ignoring-other-apps
nsapp-request-user-attention
nsscreen-main-screen
nspasteboard-general-pasteboard
nscolor-clear-color

frame
make-rect
make-size
init
init-with-content-rect
alloc
is-zoomed
convert-rect-to-backing
content-rect-for-frame-rect
set-release-when-closed
set-opaque
set-background-color
set-accepts-mouse-moved-events
set-title
set-content-view
make-first-responder-view
cg-display-bounds
cg-main-display-id
set-frame
frame-rect-for-content-rect
set-frame-origin
set-miniwindow-title
order-front
order-out
zoom
miniaturize
set-content-min-size
set-content-max-size
make-key-and-order-front
style-mask
set-style-mask
make-first-responder
set-collection-behavior
set-level
types
string-for-type
declare-types
set-string
