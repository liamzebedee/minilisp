#!/usr/local/bin/sbcl --script

(sb-ext:restrict-compiler-policy 'debug 3)
; Beginning of my experimentations in Lisp.
; I'm going to create a simple snake animation.

; This might fetch packages from the Internet.
; The path is also system-specific.
; Considered Harmful?
(load "~/.quicklisp/setup.lisp")



;; (defvar arr #(1 2 3))

(defvar arr #(1 2 3 4))


;; (defvar arr (make-array ))

;; (write arr)
; arr needs to be a (VECTOR (UNSIGNED-BYTE 8))


;; (defun hashstr (str)
;;   (byte-array-to-hex-string (digest-sequence :sha256 (ascii-string-to-byte-array str))))


;; (defvar hexstring "0123456789")
;; (write (map 
;;     'vector 
;;     #'(lambda (x) (parse-integer x :radix 16))
;;     "0123456789"
;; ))
;; #(STANDARD-CHAR STANDARD-CHAR STANDARD-CHAR STANDARD-CHAR STANDARD-CHAR


;; (write (parse-integer hexstring :start 0 :radix 16))

;; (reduce '+ '(1 2))
;; (reduce #'+ '(1 2 3 4 5))

;; (type-of (lambda (x) (+ x 1)))

;; (map 'vector 'char-code "foobar")

;; (function +)
;; (+ 1 2)

;; ((function +) (1 2))


;; 0[4] (symbol-function '+)
;; #<FUNCTION +>



;; (map 'vector #'(lambda x ) "foobar")

(defun fibbonaci (n)
    (cond 
        ((eq n 0) 0)
        ((eq n 1) 1)
        ((> n 1) (+
            (fibbonaci (- n 1))
            (fibbonaci (- n 2))
        ))
    )
)

;; (write (fibbonaci 19))




;; Now we're going to solve Jack's problem in Lisp.
;; Define a function that converts a string into its hexadecimal byte equivalent.

;; Writing Lisp means composability everywhere we go.
;; So we define the case for a single digit? 


(defun hex-string-to-bytes (str) 
    (map 
        '(vector (unsigned-byte 8)) 
        #'digit-char-p 
        str))

(defvar example-arr (hex-string-to-bytes "1234"))
(write example-arr)
(describe example-arr)



;; Checking referential transparency for Jack.

(defvar x 5)

(defun side-effects nil
    (setq x (+ x 1)))

(dotimes (n 5) 
    (format t "x: ~S~%" (side-effects)))


(defun range (n) 
    (cond ((= n 0) nil) (t (cons n (range (- n 1))))))

(write (range 5))
;; (ironclad:digest-sequence 
;;     :sha256 arr)