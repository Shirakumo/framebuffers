(in-package #:org.shirakumo.framebuffers.wayland)

(defvar *moddef-table* '((:control -1 "Control")
                         (:alt -1 "Mod1")
                         (:shift -1 "Shift")
                         (:super -1 "Mod4")
                         (:caps-lock -1 "Lock")
                         (:num-lock -1 "Mod2")
                         (:scroll-lock -1 "Mod3")
                         (:hyper -1 "Mod5")))

(defvar *keycodes* (make-array 256))
(defvar *codetable* (make-hash-table :test 'eq))

(defun init-keycodes ()
  (loop for (i k) on '(1 :escape
                       2 :1
                       3 :2
                       4 :3
                       5 :4
                       6 :5
                       7 :6
                       8 :7
                       9 :8
                       10 :9
                       11 :0
                       12 :minus
                       13 :equal
                       14 :backspace
                       15 :tab
                       16 :q
                       17 :w
                       18 :e
                       19 :r
                       20 :t
                       21 :y
                       22 :u
                       23 :i
                       24 :o
                       25 :p
                       26 :left-bracket
                       27 :right-bracket
                       28 :enter
                       29 :left-control
                       30 :a
                       31 :s
                       32 :d
                       33 :f
                       34 :g
                       35 :h
                       36 :j
                       37 :k
                       38 :l
                       39 :semicolon
                       40 :apostrophe
                       41 :grave-accent
                       42 :left-shift
                       43 :backslash
                       44 :z
                       45 :x
                       46 :c
                       47 :v
                       48 :b
                       49 :n
                       50 :m
                       51 :comma
                       52 :period
                       53 :slash
                       54 :right-shift
                       55 :kp-multiply
                       56 :left-alt
                       57 :space
                       58 :caps-lock
                       59 :f1
                       60 :f2
                       61 :f3
                       62 :f4
                       63 :f5
                       64 :f6
                       65 :f7
                       66 :f8
                       67 :f9
                       68 :f10
                       69 :num-lock
                       70 :scroll-lock
                       71 :kp-7
                       72 :kp-8
                       73 :kp-9
                       74 :kp-subtract
                       75 :kp-4
                       76 :kp-5
                       77 :kp-6
                       78 :kp-add
                       79 :kp-1
                       80 :kp-2
                       81 :kp-3
                       82 :kp-0
                       83 :kp-decimal
                       86 :world-2
                       87 :f11
                       88 :f12
                       96 :kp-enter
                       97 :right-control
                       98 :kp-divide
                       100 :right-alt
                       102 :home
                       103 :up
                       104 :page-up
                       105 :left
                       106 :right
                       107 :end
                       108 :down
                       109 :page-down
                       110 :insert
                       111 :delete
                       117 :kp-equal
                       119 :pause
                       125 :left-super
                       126 :right-super
                       127 :menu
                       183 :f13
                       184 :f14
                       185 :f15
                       186 :f16
                       187 :f17
                       188 :f18
                       189 :f19
                       190 :f20
                       191 :f21
                       192 :f22
                       193 :f23
                       194 :f24
                       210 :print-screen)
        by #'cddr
        do (setf (aref *keycodes* i) k))
  (loop for i from 0 below (length *keycodes*)
        for key = (aref *keycodes* i)
        do (when key (setf (gethash key *codetable*) i))))

(init-keycodes)

(defun translate-key (key)
  (when (<= 0 key 255)
    (aref *keycodes* key)))

(defun key-code (key)
  (gethash key *codetable*))
