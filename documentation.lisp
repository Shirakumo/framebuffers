(in-package #:org.shirakumo.framebuffers)

(docs:define-docs
  (type framebuffer-error
    "Error signalled when a backend fails to perform an operation.

See WINDOW")
  
  (function window
    "Accesses the window associated with the object.

See WINDOW (type)
See FRAMEBUFFER-ERROR (type)
See EVENT-HANDLER (type)")

  (type icon
    "Representation of the pixel data of some icon.

See MAKE-ICON
See WIDTH
See HEIGHT
See BUFFER
See ICON
See CURSOR-ICON")

  (function make-icon
    "Constructs an icon from the raw image data.

WIDTH and HEIGHT should be the dimensions of the image data in
pixels. BUFFER should be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) that
represents the image data in BGRA order.

See BUFFER
See ICON (type)")

  (type touchpoint
    "Representation of a point that is being touched on the screen.

See LOCATION
See RADIUS
See ANGLE
See PRESSURE")

  (function radius
    "Returns the radius of the touch ellipsis as a (X . Y) cons in pixels.

See TOUCHPOINT (type)")

  (function angle
    "Returns the angle of the touch ellipsis in a normalized [0 . PI] range.

See TOUCHPOINT (type)")

  (function pressure
    "Returns the pressure of the touchpoint in a normalized [0 . 1] range.

See TOUCHPOINT (type)")

  (type display
    "Representation of an output display or monitor attached to the system.

See WINDOW (type)
See LIST-DISPLAYS
See ID
See TITLE
See VIDEO-MODES
See VIDEO-MODE
See PRIMARY-P
See SIZE
See LOCATION
See WIDTH
See HEIGHT")

  (function id
    "Returns the unique ID of the display or video-mode.

Represents the display or video-mode uniquely as a string. This ID
will remain the same across restarts of the application.

See DISPLAY (type)
See VIDEO-MODE (type)")

  (function title
    "Returns a human-readable title for the display or video-mode.

This is not necessarily globally unique or the same across restarts of
the application, but it should be identifiable enough for humans.

See DISPLAY (type)
See VIDEO-MODE (type)")

  (function video-modes
    "Lists all valid video modes for the given display.

See VIDEO-MODE (type)
See DISPLAY (type)")

  (function video-mode
    "Returns the current video mode of the given display.

See VIDEO-MODE (type)
See DISPLAY (type)")

  (function primary-p
    "Returns whether the display is configured as the primary display.

See DISPLAY (type)")

  (type video-mode
    "Representation of a particular mode the display can take on.

See DISPLAY (type)
See LIST-VIDEO-MODES
See ID
See TITLE
See DISPLAY
See WIDTH
See HEIGHT
See REFRESH-RATE")

  (function display
    "Returns the display of the object.

For a VIDEO-MODE that is the display for which it was listed. For a
WINDOW, it is the display the window is currently located on, if any,
regardless of whether the window is fullscreened or not.

See DISPLAY (type)
See WINDOW (type)
See VIDEO-MODE (type)")

  (function refresh-rate
    "Returns the refresh-rate of the video mode in hertz.

See VIDEO-MODE")
  
  (function init
    "Initialises the backend.

If no backend is passed, this will try all available backends until
one is found that works. If a working one is found, its name is
returned. If none is found, an error is signalled.

If you pass a backend name explicitly (as a keyword), it will only try
that specific backend and will error if it fails to initialise for
whatever reason.

See SHUTDOWN")
  
  (function shutdown
    "Shuts the backend down (if any).

This will also close all windows that are still open.

See INIT")
  
  (type window
    "Representation of a framebuffer.

See WITH-WINDOW
See EVENT-HANDLER
See OPEN
See VALID-P
See CLOSE-REQUESTED-P
See CLOSE
See WIDTH
See HEIGHT
See SIZE
See MINIMUM-SIZE
See MAXIMUM-SIZE
See LOCATION
See TITLE
See VISIBLE-P
See MAXIMIZED-P
See ICONIFIED-P
See FOCUSED-P
See BORDERLESS-P
See ALWAYS-ON-TOP-P
See RESIZABLE-P
See FLOATING-P
See MOUSE-ENTERED-P
See FULLSCREEN-P
See CLIPBOARD
See CONTENT-SCALE
See BUFFER
See SWAP-BUFFERS
See PROCESS-EVENTS
See REQUEST-ATTENTION
See MOUSE-LOCATION
See MOUSE-BUTTON-PRESSED-P
See KEY-PRESSED-P
See ICON
See CURSOR-ICON
See CURSOR-STATE
See DISPLAY
See SET-TIMER
See CANCEL-TIMER")
  
  (type event-handler
    "Accesses the event handler of the window.

Event callbacks to the window will be delegated to this handler.
You can change the event handler at any time.

See EVENT-HANDLER
See WINDOW (type)
See WINDOW-MOVED
See WINDOW-RESIZED
See WINDOW-REFRESHED
See WINDOW-FOCUSED
See WINDOW-ICONIFIED
See WINDOW-MAXIMIZED
See WINDOW-CLOSED
See MOUSE-BUTTON-CHANGED
See MOUSE-MOVED
See MOUSE-ENTERED
See MOUSE-SCROLLED
See KEY-CHANGED
See STRING-ENTERED
See FILE-DROPPED
See CONTENT-SCALE-CHANGED
See TOUCH-STARTED
See TOUCH-MOVED
See TOUCH-ENDED
See TOUCH-CANCELLED
See PEN-MOVED
See TIMER-TRIGGERED")

  (function event-handler
    "Accesses the event handler for the window.

See WINDOW (type)
See EVENT-HANDLER (type)")
  
  (function open
    "Creates a new framebuffer window.

This implicitly calls INIT if it hasn't been called yet.
If successful returns a new WINDOW instance suitable for the selected
backend.

The window will have the following default properties unless
customised:

  EVENT-HANDLER   --- EVENT-HANDLER
  SIZE            --- (NIL . NIL)          Fills the screen
  LOCATION        --- (NIL . NIL)          Centers on screen
  TITLE           --- \"Framebuffer\"
  VISIBLE-P       --- T
  MAXIMIZED-P     --- NIL
  ICONIFIED-P     --- NIL
  MINIMUM-SIZE    --- (1 . 1)
  MAXIMUM-SIZE    --- (NIL . NIL)
  FOCUSED-P       --- T
  BORDERLESS-P    --- NIL
  ALWAYS-ON-TOP-P --- NIL
  FULLSCREEN-P    --- NIL
  RESIZABLE-P     --- T
  FLOATING-P      --- NIL
  ICON            --- NIL
  CURSOR-STATE    --- :NORMAL
  CURSOR-ICON     --- :ARROW

See INIT
See WINDOW (type)
See WITH-WINDOW")
  
  (function with-window
    "Convenience function to establish a window and event loop.

WINDOW is bound to the window instance.
INITARGS are argument spassed to OPEN.
HANDLERS should be event handler bodies:

  HANDLER    ::= (event-type (arg...) . body)
               | (T (event-name arg...) . body)
  EVENT-TYPE --- The name of an event-handler function
  EVENT-NMAE --- Variable bound to the name of the event type called.
  ARG        --- Varibale bound to the respective event type argument.
  BODY       --- Body forms to execute when the event is handled.

See WINDOW (type)
See EVENT-HANDLER (type)")
  
  (function do-pixels
    "Iterate over the pixels in the buffer.

BUF is bound to the (USIGNED-BYTE 8) array
I   is bound to the pixel starting index
X   is bound to the pixel's X coordinate
Y   is bound to the pixel's Y coordinate

WINDOW should be the framebuffer window to iterate over.

BODY are forms to be executed for each pixel.

Each pixel is stored as a quadruplet of (UNSIGNED-BYTE 8) values, in
the order of Blue Green Red Alpha, with the first pixel being in the
top left corner of the framebuffer, growing in X then Y.

See WINDOW (type)")

  (function clear
    "Clears the pixel buffer to :white or :black.

This is equivalent to (fill buffer 0) for black or (fill buffer 255)
for white, but may be more efficient.

See BUFFER")
  
  (function valid-p
    "Returns whether the window is usable or not.

See CLOSE
See WINDOW (type)")
  
  (function close-requested-p
    "Accesses whether the user has requested a close of the window.

You should set this to NIL again to avoid an automatic closure of the
window.

See WINDOW (type)")
  
  (function close
    "Closes and deallocates the window permanently.

After this the window will no longer be valid and cannot be used
further.

See VALID-P
See WINDOW (type)")
  
  (function width
    "Returns the width of the object in pixels.

See WINDOW (type)
See ICON (type)
See DISPLAY (type)")
  
  (function height
    "Returns the height of the object in pixels.

See WINDOW (type)
See ICON (type)
See DISPLAY (type)")
  
  (function size
    "Accesses the size of the object as a (W . H) cons in pixels.

See WINDOW (type)
See DISPLAY (type)")

  (function minimum-size
    "Accesses the minimum size of the window as a (W . H) cons in pixels.

W and H must be integers above 0.

See WINDOW (type)")

  (function maximum-size
    "Accesses the maximum size of the window as a (W . H) cons in pixels.

W and H must be integers above 0 or NIL to denote no limit.

See WINDOW (type)")
  
  (function location
    "Accesses the location of the object as a (X . Y) cons in pixels.

[Wayland] The window location cannot be accessed and will simply
          return (0 . 0) always.

See WINDOW (type)
See TOCHPOINT (type)
See DISPLAY (type)")
  
  (function title
    "Accesses the title of the window.

See WINDOW (type)")
  
  (function visible-p
    "Accesses whether the window is visible at all.

See WINDOW (type)")
  
  (function maximized-p
    "Accesses whether the window should be maximized or not.

See WINDOW (type)")
  
  (function iconified-p
    "Accesses whether the window should be iconified/minimized or not.

[Wayland] The window cannot be un-minimized programmatically. Trying
          to do so will simply be a no-op.

See WINDOW (type)")

  (function focused-p
    "Accesses whether the window has focus or not.

When setting this property, it can usually only forcibly be set to true.

See WINDOW (type)")
  
  (function borderless-p
    "Accesses whether the window should have a border or decorations, or not.

See WINDOW (type)")
  
  (function always-on-top-p
    "Accesses whether the window should always stick on top or not.

See WINDOW (type)")
  
  (function resizable-p
    "Accesses whether the window is meant to be resizable or not.

Note that even when set not to be, your window may be forced to a
particular size by the windowing system.

See WINDOW (type)")
  
  (function floating-p
    "Accesses whether the window is set to be floating or not.

See WINDOW (type)")
  
  (function mouse-entered-p
    "Returns whether the mouse is currently in the window or not.

See WINDOW (type)")

  (function fullscreen-p
    "Returns whether the window is currently in fullscreen mode or not.

When setting this place, you may pass one of the following:

  NIL         --- Returns the window to its original state prior to
                  fullscreening.
  T           --- Fullscreens on the current DISPLAY without changing
                  its current VIDEO-MODE. If the window has no current
                  display, the display that is PRIMARY-P is used.
  DISPLAY     --- Fullscreens on the given DISPLAY without changing
                  its current VIDEO-MODE.
  VIDEO-MODE  --- Fullscreens on the display of the VIDEO-MODE and if
                  necessary switches it to that mode.
  STRING      --- Fullscreens on the VIDEO-MODE or DISPLAY that has an
                  ID that's STRING= to the passed string. If none is
                  found, this is the same as T.

See DISPLAY (type)
See VIDEO-MODE (type)
See WINDOW (type)")
  
  (function clipboard
    "Accesses the clipboard contents.

Which types of clipboard contents are supported varies by
backend. Ideally it should support getting and setting the following
value types:

  STRING    --- Basic text
  PATHNAME  --- A file
  ICON      --- An image
  NULL      --- Empty clipboard

See WINDOW (type)")
  
  (function content-scale
    "Returns the content scaling factor the OS reports for the window as a (XSCALE . YSCALE) cons.

Ideally you should scale your window contents by these factors for
optimal DPI-aware display.

See WINDOW (type)")
  
  (function buffer
    "Returns the framebuffer or icon contents as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

The framebuffer is guaranteed to have a length of (* 4 WIDTH HEIGHT),
with the pixels stored in row(width)-major order and the first pixel
being in the top-left corner.

Each pixel is stored as a quadruplet of (UNSIGNED-BYTE 8) values, in
the order of Blue Green Red Alpha.

See WINDOW (type)
See ICON (type)")
  
  (function swap-buffers
    "Swaps the framebuffer to display its contents to the user.

X, Y, W, H may be used to designate a sub-region of the framebuffer to
swap for more efficient updates.
If SYNC is T, the function will block until the swap has flushed
through. Otherwise it may return immediately before the contents are
visible.

See WINDOW (type)")
  
  (function process-events
    "Processes pending events for the window.

This will cause the event handler functions to be invoked for any
pending events that may be queued for the window.

TIMEOUT may be one of the following:
  NIL  --- Process only immediately pending events.
  T    --- Process events in a loop until the window is closed or
           CLOSE-REQUESTED-P is true.
  REAL --- A real number representing the maximum number of seconds to
           wait for events to arrive. If there are pending events
           already this option is equivalent to NIL.

See WINDOW (type)")
  
  (function request-attention
    "Request the user to pay attention to the window.

This usually makes the window or its icon representation flash in some
way.

See WINDOW (type)")

  (function mouse-location
    "Returns the last known mouse location inside the window as a (X . Y) cons in pixels.

See WINDOW (type)")

  (function mouse-button-pressed-p
    "Returns the last known mouse button state.

See WINDOW (type)
See MOUSE-BUTTON-CHANGED")

  (function key-pressed-p
    "Returns the last known key state.

See WINDOW (type)
See KEY-CHANGED")

  (function key-scan-code
    "Returns the scan code for a particular key symbol.

If no translation is known for the given key, NIL is returned.

See WINDOW (type)
See KEY-CHANGED")

  (function local-key-string
    "Returns a string representing the icon on the physical key represented by the particular key symbol.

This will perform an inverse translation based on the user's keyboard
layout and try to present a matching human-readable representation of
that key's label.

For instance, on a DVORAK layout, passing in :I will return \"C\"
while on a standard US QWERTY layout it will return \"I\".

If no translation is known for the given key, NIL is returned.

See WINDOW (type)
See KEY-CHANGED")

  (function icon
    "Accesses the icon of the window.

Returns NIL if no icon was set previously.
The call may fail if the ICON instance has unsuitable dimensions for
the backend.

See ICON (type)
See WINDOW (type)")

  (function cursor-icon
    "Accesess the icon of the cursor in the window.

May return an ICON instance if one was set explicitly by the user, or
one of the following keywords defining default cursor types:

  NIL
  :ARROW         --- The default pointer/arrow cursor shape
  :IBEAM         --- A crossbeam usually for text input
  :CROSSHAIR     --- A crosshair usually for precise selection
  :POINTING-HAND --- A pointing hand to designate interactivity or
                     dragging
  :RESIZE-EW     --- Shapes to indicate the directions in which 
  :RESIZE-NS         something may be resized
  :RESIZE-NWSE   
  :RESIZE-NESW   
  :RESIZE-ALL    
  :NOT-ALLOWED   --- To indicate a deactivated or forbidden
                     interaction

You may also set the cursor-icon in the same way. A backend may also
support additional icon types, though only the above are required.

The call may fail if the ICON intsance has unsuitable dimensions for
the backend.

See ICON (type)
See WINDOW (type)")

  (function cursor-state
    "Accesses the state of the cursor behaviour.

The state may be one of the following:

  :NORMAL   --- The cursor behaves as normal
  :HIDDEN   --- The cursor is not displayed over the window
  :CAPTURED --- The cursor cannot leave the window

See WINDOW (type)")

  (function set-timer
    "Creates a new timer object and returns it.

DELAY is the number of seconds after which TIMER-TRIGGERED is
invoked for the returned timer object. If REPEAT is true, then the
callback is repeatedly invoked in every DELAY interval until
CANCEL-TIMER is called for the timer, or the WINDOW is closed.

Note that how accurate the callback is is dependent on how often
PROCESS-EVENTS is called.

See WINDOW (type)
See CANCEL-TIMER
See TIMER-TRIGGERED
See PROCESS-EVENTS")

  (function cancel-timer
    "Cancels the timer object.

After this call, no further TIMER-TRIGGERED callbacks will be invoked
for the given timer object.

See WINDOW (type)
See SET-TIMER
See TIMER-TRIGGERED")
  
  (function window-moved
    "Callback for when the window has moved on screen.

See LOCATION
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-resized
    "Callback for when the window and framebuffer have been resized.

See SIZE
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-refreshed
    "Callback for when the window has been damaged in some way and should have its contents redrawn.

You should probably invoke SWAP-BUFFERS from this.

See SWAP-BUFFERS
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-focused
    "Callback for when the window's focus state has changed.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-iconified
    "Callback for when the window's iconified/minimized status has changed.

See ICONIFIED-P
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-maximized
    "Callback for when the window's maximized status has changed.

See MAXIMIZED-P
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function window-closed
    "Callback for when the window is requested to be closed by the user.

On call CLOSE-REQUESTED-P will be set to T. You should set it back to
NIL if you would like to reject the window close request.

See CLOSE-REQUESTED-P
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function mouse-button-changed
    "Callback for when a mouse button has changed.

BUTTON may be one of:
  :LEFT   --- The left mouse button
  :RIGHT  --- The right mouse button
  :MIDDLE --- The middle/scroll wheel mouse button
  integer --- An extra button identified by its number

ACTION may be one of:
  :PRESS        --- The button is now held down
  :RELEASE      --- The button is no longer held down
  :DOUBLE-CLICK --- The button has been clicked twice in succession

MODIFIERS is a set of the following modifiers that wer active at the
time:
  :CONTROL
  :ALT
  :SHIFT
  :SUPER
  :HYPER
  :CAPS-LOCK
  :NUM-LOCK
  :SCROLL-LOCK

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function mouse-moved
    "Callback for when the mouse cursor has moved over the window.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function mouse-entered
    "Callback for when the mouse cursor has entered or left the window.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function mouse-scrolled
    "Callback for when the mouse scroll wheel has moved.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function key-changed
    "Callback for when a key's state has changed.

KEY is a keyword description of the ANSI-US label of the key that was
pressed, or NIL if no description is known. In addition to the basic
alphanumerics, this library also defines the following key names:

  :F1
  :F2
  :F3
  :F4
  :F5
  :F6
  :F7
  :F8
  :F9
  :F10
  :F11
  :F12
  :F13
  :F14
  :F15
  :F16
  :F17
  :F18
  :F19
  :F20
  :F21
  :F22
  :F23
  :F24
  :KP-7
  :KP-8
  :KP-9
  :KP-4
  :KP-5
  :KP-6
  :KP-1
  :KP-2
  :KP-3
  :KP-0
  :KP-SUBTRACT
  :KP-ADD
  :KP-DECIMAL
  :KP-EQUAL
  :KP-ENTER
  :KP-DIVIDE
  :KP-MULTIPLY
  :LEFT-ALT
  :LEFT-BRACKET
  :LEFT-CONTROL
  :LEFT-SHIFT
  :LEFT-SUPER
  :RIGHT-ALT
  :RIGHT-BRACKET
  :RIGHT-CONTROL
  :RIGHT-SHIFT
  :RIGHT-SUPER
  :APOSTROPHE
  :BACKSLASH
  :BACKSPACE
  :CAPS-LOCK
  :COMMA
  :DELETE
  :DOWN
  :END
  :ENTER
  :EQUAL
  :ESCAPE
  :GRAVE-ACCENT
  :HOME
  :INSERT
  :LEFT
  :MENU
  :MINUS
  :NUM-LOCK
  :PAGE-DOWN
  :PAGE-UP
  :PAUSE
  :PERIOD
  :PRINT-SCREEN
  :RIGHT
  :SCROLL-LOCK
  :SEMICOLON
  :SLASH
  :SPACE
  :TAB
  :UP
  :WORLD-2

SCAN-CODE is the internal integer code of the key that was pressed.

ACTION may be one of:
  :PRESS   --- The key is now held down
  :RELEASE --- The key is no longer held down
  :REPEAT  --- The key is still left down and is being repeated by the
               system

MODIFIERS is a set of the following modifiers that wer active at the
time:
  :CONTROL
  :ALT
  :SHIFT
  :SUPER
  :HYPER
  :CAPS-LOCK
  :NUM-LOCK
  :SCROLL-LOCK

Note that you should not use this function to process textual input,
as it is not aware of keyboard layout translation mechanisms. Use
STRING-ENTERED instead for such purposes.

See STRING-ENTERED
See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function string-entered
    "Callback for when some text has been entered into the window.

This text has been properly processed by whatever keyboard layout
translation mechanisms or input method translations may be
applicable.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function file-dropped
    "Callback for when one or more files have been dragged and dropped onto the window.

See EVENT-HANDLER (type)
See WINDOW (type)")
  
  (function content-scale-changed
    "Callback for when the desired content-scale of the window has changed.

This can occur due to a monitor reconfiguration or the window being
moved to another monitor with different requirements.

See CONTENT-SCALE
See EVENT-HANDLER (type)
See WINDOW (type)")

  (function touch-started
    "Callback for when a touch event has started.

The POINTS is a list of TOUCHPOINT instances describing the contact
points and their shape on the screen.

See EVENT-HANDLER (type)
See WINDOW (type)
See TOUCH-POINT (type)")

  (function touch-moved
    "Callback for when a touch event has changed.

The POINTS is a list of TOUCHPOINT instances describing the contact
points and their shape on the screen. The points in the set will be EQ
to the ones from the previous TOUCH-MOVED or TOUCH-STARTED if they are
tracked to be the same points. The set may change as touchpoints are
removed or added druing the same motion.

See EVENT-HANDLER (type)
See WINDOW (type)
See TOUCH-POINT (type)")

  (function touch-ended
    "Callback for when a touch event has successfully completed.

This is called when all touchpoints have left the screen. The set of
POINTS is the last before this happened.

See EVENT-HANDLER (type)
See WINDOW (type)
See TOUCH-POINT (type)")

  (function touch-cancelled
    "Callback fro when a touch event has bene interrupted by some other system action.

This should be used to cancel an operation that was started by the
touch event.The set of POINTS is the last before this happened.

See EVENT-HANDLER (type)
See WINDOW (type)
See TOUCH-POINT (type)")

  (function pen-moved
    "Callback for when a stylus or pen has been moved across the screen.

This is in addition to a mouse event, and carries the following extra
information:

  MODE     --- Either :NORMAL or :ERASER
  PRESSURE --- A normalised pressure value in the range [0 . 1]
  XTILT    --- The tilt of the pen in X direction along the screen as
               an angle in the range [0 . PI]
  YTILT    --- The tilt of the pen in Y direction along the screen as
               an angle in the range [0 . PI]

See EVENT-HANDLER (type)
See WINDOW (type)")

  (function timer-triggered
    "Callback for when a timer has reached its timeout.

This will be called repeatedly for the same timer if the timer is set
to repeat. For non-repeating timers it is only called once.

Note that how accurate the callback is to the timer's timeout is
dependent on how often PROCESS-EVENTS is called.

See EVENT-HANDLER (type)
See SET-TIMER
See CANCEL-TIMER
See PROCESS-EVENTS"))
