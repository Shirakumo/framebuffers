(in-package #:org.shirakumo.framebuffers.cocoa.cffi)

(cffi:defbitfield style-mask
  (:borderless      0)
  (:titled          1)
  (:closable        2)
  (:miniaturizable  4)
  (:resizable       8)
  (:utility         16)
  (:docmodal        32)
  (:icon            64)
  (:mini            128))

(cffi:defbitfield collection-behavior
  (:default 0)
  (:can-join-all-spaces 1)
  (:move-to-active-space 2)
  (:managed 4)
  (:transient 8)
  (:stationary 16)
  (:participates-in-cycle 32)
  (:ignores-cycle 64)
  (:full-screen-primary 128)
  (:full-screen-auxiliary 256)
  (:full-screen-none 512)
  (:full-screen-allows-tiling 2048)
  (:full-screen-disallows-tiling 4096))

(cffi:defbitfield bitmap-info
  (:alpha-info-mask 31)
  (:byte-order-16-big 12288)
  (:byte-order-16-little 4096)
  (:byte-order-32-big 16384)
  (:byte-order-32-little 8192)
  (:byte-order-default 0)
  (:byte-order-mask 28672)
  (:first 4)
  (:float-components 256)
  (:float-info-mask 3840)
  (:last 3)
  (:none 0)
  (:none-skip-first 6)
  (:none-skip-last 5)
  (:only 7)
  (:premultiplied-first 2)
  (:premultiplied-last 1))

(cffi:defcenum rendering-intent
  (:default 0)
  :absolute-colorimetric
  :relative-colorimetric
  :perceptual
  :saturation)

(cffi:defcenum window-level-key
  (:base 0)
  :minimum
  :desktop
  :backstop-menu
  :normal
  :floating
  :torn-off-menu
  :dock
  :main-menu
  :status
  :modal-panel
  :popup-menu
  :dragging
  :screensaver
  :maximum
  :overlay
  :help
  :utility
  :desktop-icon
  :cursor
  :assistive-tech-high)

(cffi:defcvar (paste-url "NSPasteboardTypeURL") :pointer)
(cffi:defcvar (paste-collaboration-metadata "NSPasteboardTypeCollaborationMetadata") :pointer)
(cffi:defcvar (paste-color "NSPasteboardTypeColor") :pointer)
(cffi:defcvar (paste-file-url "NSPasteboardTypeFileURL") :pointer)
(cffi:defcvar (paste-font "NSPasteboardTypeFont") :pointer)
(cffi:defcvar (paste-html "NSPasteboardTypeHTML") :pointer)
(cffi:defcvar (paste-multiple-text-selection "NSPasteboardTypeMultipleTextSelection") :pointer)
(cffi:defcvar (paste-pdf "NSPasteboardTypePDF") :pointer)
(cffi:defcvar (paste-png "NSPasteboardTypePNG") :pointer)
(cffi:defcvar (paste-rtf "NSPasteboardTypeRTF") :pointer)
(cffi:defcvar (paste-rtfd "NSPasteboardTypeRTFD") :pointer)
(cffi:defcvar (paste-ruler "NSPasteboardTypeRuler") :pointer)
(cffi:defcvar (paste-sound "NSPasteboardTypeSound") :pointer)
(cffi:defcvar (paste-string "NSPasteboardTypeString") :pointer)
(cffi:defcvar (paste-tabular-text "NSPasteboardTypeTabularText") :pointer)
(cffi:defcvar (paste-text-finder-options "NSPasteboardTypeTextFinderOptions") :pointer)
(cffi:defcvar (paste-tiff "NSPasteboardTypeTIFF") :pointer)

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

(objc:define-objcfun "NSApplication" (nsapplication-shared-application "sharedApplication") :void)

(objc:define-objcfun "NSApp" (nsapp-set-activation-policy "setActivationPolicy:") :void
  (policy :pointer))

(objc:define-objcfun "NSApp" (nsapp-activate-ignoring-other-apps "activateIgnoringOtherApps:") :void
  (flag :bool))

(objc:define-objcfun "NSApp" (nsapp-request-user-attention "requestUserAttention:") :void
  (type :pointer))

(objc:define-objcfun "NSScreen" (nsscreen-main-screen "mainScreen") :pointer)

(objc:define-objcfun "NSPasteboard" (nspasteboard-general-pasteboard "generalPasteboard") :pointer)

(objc:define-objcfun "NSColor" (nscolor-clear-color "clearColor") :pointer)

(defmacro alloc (cls)
  `(objc:call ,cls "alloc"))

(objc:define-objcmethod (init "init") :pointer)

(objc:define-objcmethod (init-with-content-rect "initWithContentRect:styleMask:backing:defer:") :pointer
  (rect (:struct rect))
  (style-mask style-mask)
  (backing :pointer)
  (defer :boolean))

(objc:define-objcmethod (frame "frame") (:struct rect))

(objc:define-objcmethod (close "close") :void)

(objc:define-objcmethod (is-zoomed "isZoomed") :boolean)

(objc:define-objcmethod (convert-rect-to-backing "convertRectToBacking:") (:struct rect)
  (rect (:struct rect)))

(objc:define-objcmethod (content-rect-for-frame-rect "contentRectForFrameRect:") (:struct rect)
  (frame (:struct rect)))

(objc:define-objcmethod (set-release-when-closed "setReleaseWhenClosed:") :void
  (value :boolean))

(objc:define-objcmethod (set-opaque "setOpaque:") :void
  (value :boolean))

(objc:define-objcmethod (set-background-color "setBackgroundColor:") :void
  (value :pointer))

(objc:define-objcmethod (set-accepts-mouse-moved-events "setAcceptsMouseMovedEvents:") :void
  (value :boolean))

(objc:define-objcmethod (set-title "setTitle:") :void
  (value objc:nsstring))

(objc:define-objcmethod (set-content-view "setContentView:") :void
  (view :pointer))

(objc:define-objcmethod (make-first-responder-view "makeFirstResponder:") :void
  (view :pointer))

(objc:define-objcmethod (set-frame "setFrame:display:") :void
  (frame (:struct rect))
  (display :boolean))

(objc:define-objcmethod (frame-rect-for-content-rect "frameRectForContentRect:") (:struct rect)
  (rect (:struct rect)))

(objc:define-objcmethod (set-frame-origin "setFrameOrigin:") :void
  (point (:struct point)))

(objc:define-objcmethod (set-miniwindow-title "setMiniwindowTitle") :void
  (title objc:nsstring))

(objc:define-objcmethod (order-front "orderFront:") :void
  (value :boolean))

(objc:define-objcmethod (order-out "orderOut:") :void
  (value :boolean))

(objc:define-objcmethod (zoom "zoom:") :void
  (value :boolean))

(objc:define-objcmethod (miniaturize "miniaturize:") :void
  (value :boolean))

(objc:define-objcmethod (deminiaturize "deminiaturize:") :void
  (value :boolean))

(objc:define-objcmethod (set-content-min-size "setContentMinSize:") :void
  (value (:struct size)))

(objc:define-objcmethod (set-content-max-size "setContentMaxSize:") :void
  (value (:struct size)))

(objc:define-objcmethod (make-key-and-order-front "makeKeyAndOrderFront:") :void
  (value :boolean))

(objc:define-objcmethod (style-mask "styleMask") style-mask)

(objc:define-objcmethod (set-style-mask "setStyleMask:") :void
  (mask style-mask))

(objc:define-objcmethod (make-first-responder "makeFirstResponder:") :void
  (target :pointer))

(objc:define-objcmethod (set-collection-behavior "setCollectionBehavior:") :void
  (behavior collection-behavior))

(objc:define-objcmethod (set-level "setLevel:") :void
  (level :int32))

(objc:define-objcmethod (types "types") objc:cfarray)

(objc:define-objcmethod (string-for-type "stringForType:") objc:nsstring
  (type :pointer))

(objc:define-objcmethod (declare-types "declareTypes:owner:") :void
  (types objc:cfarray)
  (owner :boolean))

(objc:define-objcmethod (set-string "setString") :void
  (string objc:nsstring)
  (for-type :pointer))

(objc:define-objcmethod (cgcontext "CGContext") :pointer)

(objc:define-objcfun "NSGraphicsContext" (nsgraphicscontext-current-context "currentContext") :pointer)

(cffi:defcfun (cg-display-bounds "CGDisplayBounds") (:struct rect)
  (display-id :uint32))

(cffi:defcfun (cg-main-display-id "CGMainDisplayID") :uint32)

(cffi:defcfun (window-level-for-key "CGWindowLevelForKey") :int32
  (key window-level-key))

(cffi:defcfun (cg-color-space-create-device-rgb "CGColorSpaceCreateDeviceRGB") :pointer)

(cffi:defcfun (cg-data-provider-create-with-data "CGDataProviderCreateWithData") :pointer
  (info :pointer)
  (data :pointer)
  (length :size)
  (release-callback :pointer))

(cffi:defcfun (cg-image-create "CGImageCreate") :pointer
  (width :size)
  (height :size)
  (channel-bits :size)
  (pixel-bits :size)
  (row-bytes :size)
  (color-space :pointer)
  (bitmap-info bitmap-info)
  (data-provider :pointer)
  (decoder :pointer)
  (should-interpolate :boolean)
  (intent rendering-intent))

(cffi:defcfun (cg-color-space-release "CGColorSpaceRelease") :void
  (space :pointer))

(cffi:defcfun (cg-data-provider-release "CGDataProviderRelease") :void
  (provider :pointer))

(cffi:defcfun (cg-context-draw-image "CGContextDrawImage") :void
  (context :pointer)
  (rect (:struct rect))
  (image :pointer))

(cffi:defcfun (cg-image-release "CGImageRelease") :void
  (image :pointer))

(define-symbol-macro floating-window-level (window-level-for-key :floating))

(define-symbol-macro normal-window-level (window-level-for-key :normal))

(define-symbol-macro main-menu-window-level (window-level-for-key :main-menu))
