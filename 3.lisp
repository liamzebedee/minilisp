#!/usr/local/bin/sbcl --script

(sb-ext:restrict-compiler-policy 'debug 3)
; Beginning of my experimentations in Lisp.
; I'm going to create a simple snake animation.

; This might fetch packages from the Internet.
; The path is also system-specific.
; Considered Harmful?
(load "~/.quicklisp/setup.lisp")
(map nil 'ql:quickload '(:ironclad :trivial-http))


(defvar arr #(1 2 3))
(ironclad:digest-sequence 
    :sha256 arr)
