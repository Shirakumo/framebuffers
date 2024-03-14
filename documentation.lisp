(in-package #:org.shirakumo.framebuffers.int)

(docs:define-docs
  (type framebuffer-error
    "Error signalled when a backend fails to perform an operation.

See WINDOW")
  
  (function window
    "Accesses the window associated with the object.

See WINDOW (type)
See FRAMEBUFFER-ERROR (type)
See EVENT-HANDLER (type)")
  
  (function init
    "Initialises the backend.

This will try all available backends until one is found that works. If
a working one is found, its name is returned. If none is found, an
error is signalled.

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
See LOCATION
See TITLE
See VISIBLE-P
See MAXIMIZED-P
See ICONIFIED-P
See CLIPBOARD-STRING
See CONTENT-SCALE
See BUFFER
See SWAP-BUFFERS
See PROCESS-EVENTS
See REQUEST-ATTENTION")
  
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
See CONTENT-SCALE-CHANGED")

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
  CURSOR-ICON     --- :POINTER

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
the order of Blue Green Red Alpha.

See WINDOW (type)")
  
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
    "Returns the width of the window in pixels.

See WINDOW (type)")
  
  (function height
    "Returns the height of the window in pixels.

See WINDOW (type)")
  
  (function size
    "Accesses the size of the window as a (W . H) cons in pixels.

See WINDOW (type)")
  
  (function location
    "Accesses the location of the window as a (X . Y) cons in pixels.

[Wayland] The window location cannot be accessed and will simply
          return (0 . 0) always.

See WINDOW (type)")
  
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
  
  (function clipboard-string
    "Accesses the clipboard contents.

See WINDOW (type)")
  
  (function content-scale
    "Returns the content scaling factor the OS reports for the window as a (XSCALE . YSCALE) cons.

Ideally you should scale your window contents by these factors for
optimal DPI-aware display.

See WINDOW (type)")
  
  (function buffer
    "Returns the framebuffer contents as an (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

The framebuffer is guaranteed to have a length of (* 4 WIDTH HEIGHT),
with the pixels stored in row(width)-major order. 

Each pixel is stored as a quadruplet of (UNSIGNED-BYTE 8) values, in
the order of Blue Green Red Alpha.

See WINDOW (type)")
  
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
See WINDOW (type)"))
