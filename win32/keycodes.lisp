(in-package #:org.shirakumo.framebuffers.win32)

(defvar *keycodes* (make-array 356))

(defun init-keycodes ()
  (loop for (i k) on '(#x001 :escape
                       #x002 :1
                       #x003 :2
                       #x004 :3
                       #x005 :4
                       #x006 :5
                       #x007 :6
                       #x008 :7
                       #x009 :8
                       #x00a :9
                       #x00b :0
                       #x00c :minus
                       #x00d :equal
                       #x00e :backspace
                       #x00f :tab
                       #x010 :q
                       #x011 :w
                       #x012 :e
                       #x013 :r
                       #x014 :t
                       #x015 :y
                       #x016 :u
                       #x017 :i
                       #x018 :o
                       #x019 :p
                       #x01a :left-bracket
                       #x01b :right-bracket
                       #x01c :enter
                       #x01d :left-control
                       #x01e :a
                       #x01f :s
                       #x020 :d
                       #x021 :f
                       #x022 :g
                       #x023 :h
                       #x024 :j
                       #x025 :k
                       #x026 :l
                       #x027 :semicolon
                       #x028 :apostrophe
                       #x029 :grave-accent
                       #x02a :left-shift
                       #x02b :backslash
                       #x02c :z
                       #x02d :x
                       #x02e :c
                       #x02f :v
                       #x030 :b
                       #x031 :n
                       #x032 :m
                       #x033 :comma
                       #x034 :period
                       #x035 :slash
                       #x036 :right-shift
                       #x037 :kp-multiply
                       #x038 :left-alt
                       #x039 :space
                       #x03a :caps-lock
                       #x03b :f1
                       #x03c :f2
                       #x03d :f3
                       #x03e :f4
                       #x03f :f5
                       #x040 :f6
                       #x041 :f7
                       #x042 :f8
                       #x043 :f9
                       #x044 :f10
                       #x045 :pause
                       #x046 :scroll-lock
                       #x047 :kp-7
                       #x048 :kp-8
                       #x049 :kp-9
                       #x04a :kp-subtract
                       #x04b :kp-4
                       #x04c :kp-5
                       #x04d :kp-6
                       #x04e :kp-add
                       #x04f :kp-1
                       #x050 :kp-2
                       #x051 :kp-3
                       #x052 :kp-0
                       #x053 :kp-decimal
                       #x056 :world-2
                       #x057 :f11
                       #x058 :f12
                       #x059 :kp-equal
                       #x064 :f13
                       #x065 :f14
                       #x066 :f15
                       #x067 :f16
                       #x068 :f17
                       #x069 :f18
                       #x06a :f19
                       #x06b :f20
                       #x06c :f21
                       #x06d :f22
                       #x06e :f23
                       #x076 :f24
                       #x11c :kp-enter
                       #x11d :right-control
                       #x135 :kp-divide
                       #x137 :print-screen
                       #x138 :right-alt
                       #x145 :num-lock
                       #x147 :home
                       #x148 :up
                       #x149 :page-up
                       #x14b :left
                       #x14d :right
                       #x14f :end
                       #x150 :down
                       #x151 :page-down
                       #x152 :insert
                       #x153 :delete
                       #x15b :left-super
                       #x15c :right-super
                       #x15d :menu)
        by #'cddr
        do (setf (aref *keycodes* i) k)))

(init-keycodes)

(defun translate-keycode (keycode)
  (when (<= 0 keycode (1- 356))
    (aref *keycodes* keycode)))

;; TODO: implement key name lookup
