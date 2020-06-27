#!/usr/local/bin/sbcl --script

; Beginning of my experimentations in Lisp.
; I'm going to create a simple snake animation.

; This might fetch packages from the Internet.
; The path is also system-specific.
; Considered Harmful?
(load "~/.quicklisp/setup.lisp")


;; (defpackage #:liamslisp
;;     (:use #:ironclad ; crypto
          
;;     ))


(map nil 'ql:quickload '(:ironclad :trivial-http))



(defvar arr #(1 2 3))
;; (write (type-of arr))
;; (write 
;;     (type-of 
;;         (make-array 10
;;             :element-type '(unsigned-byte 8)
;;             :initial-element 0)))


;; (write 'unsigned-byte)
;; (write arr)

(print
    "These types are equal:")


(defvar arr1 (make-array 2 ; len=2
            :element-type '(unsigned-byte 8)
            :initial-element 0))

;; (print (simple-array (unsigned-byte 8) (0))

(defun octet-vector (&optional length)
    `(simple-array (unsigned-byte 8) (,length)))

(print 
    (octet-vector 2))

;; (
    

    (defvar 
        arr2 
        ;; (octet-vector 8))
        #((integer 0 0) (integer 0 0)))
;; )



;; (mapc 'print '(1 2 3 4 5))

;; (write (type-of '(2)))

(setq *print-pretty* '1)

;; (print (list arr1))

(print 
    (map
        'list 
        'type-of 
        (list arr1 arr2)))



; ((SIMPLE-ARRAY (UNSIGNED-BYTE 8) (2)) (SIMPLE-VECTOR 2)) 

(print 
    (eq
        'arr1 'arr2
        ))

(print
    (
        list 
        (car '(1 2 3))
        (cdr '(1 2 3))
        (cons '(1 2 3) '(1 2 3))
    )
)

;; (cdr (1 2 3))

; arr needs to be a (VECTOR (UNSIGNED-BYTE 8))
(ironclad:digest-sequence 
    :sha256 arr)




(VECTOR (UNSIGNED-BYTE 8))
(SIMPLE-VECTOR 2)

;; make-array


;; Make a simple binary tree in lisp.



;; (in-package #:cl-user)

;; (defpackage #:reddit
;;   (:use #:cl
;;         #:tbnl
;;         #:cl-ppcre
;;         #:trivial-http
;;         #:cl-who
;;         #:clsql-user
;;         #:cl-smtp
;;         #:ironclad))

