Installed Lisp.
https://lisp-lang.org/learn/getting-started/

Installed QuickLisp.
https://stackoverflow.com/questions/40571533/how-to-install-a-package-with-quicklisp

Lisp files can be compiled down to object code and executed.
Or executed in "script" mode.

My main unknowns?
- what is Lisp


`defvar` and `setq` are different.


S-Expressions are cooked.
https://stackoverflow.com/questions/45526302/how-to-create-a-common-lisp-sbcl-vector-of-a-particular-type-for-usage-in-a-us


(typeof )
https://stackoverflow.com/questions/10900600/how-to-determine-the-datatype-of-a-variable

The reader macro - #'
https://stackoverflow.com/questions/14021965/the-in-common-lisp

> Since you are returning it as a value, you have to write (FUNCTION (LAMBDA ...)). #'(lambda ...) is a notation which is shorter, but results in the same - using the reader macro #':

> `CL-USER 74 > (read-from-string "#'(lambda (foo) (1+ foo))")`
> `(FUNCTION (LAMBDA (FOO) (1+ FOO)))`


The backtick and comma operators.
https://stackoverflow.com/questions/30150186/what-does-backtick-mean-in-lisp

The backtick/backquote disables evaluation for every subexpression not preceded by a comma for the list that follows the operator.

> A single quote followed by the written representation of a value will produce that value:
> Suppose now that I don't want a literal symbol x in the list. I have a variable x in my program, and I want to insert the value to which x is bound.
> To mark that I want the value of x rather than the symbol x, I insert a comma before x:

(defun octet-vector (&optional length)
    `(simple-array (unsigned-byte 8) (,length)))


Integers - 
https://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html

*var* vs var
https://stackoverflow.com/questions/11932876/whats-difference-between-var-and-var-when-using-defvar


Arrays
http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm
http://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm#simple-array

Equality
https://eli.thegreenplace.net/2004/08/08/equality-in-lisp

Map
http://clhs.lisp.se/Body/f_map.htm



How does type checking work? 

char-code



Wow I didn't know that there were "functional" and "regular" values.
Apparently I use Lisp v2.
https://stackoverflow.com/questions/4578574/what-is-the-difference-between-lisp-1-and-lisp-2
http://www.nhplace.com/kent/Papers/Technical-Issues.html


http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm#format
format function

### Cells, cons, cdr

https://en.wikipedia.org/wiki/CAR_and_CDR
https://www.gnu.org/software/emacs/manual/html_node/elisp/Cons-Cells.html


cons constructs memory objects which hold two values or pointers to values

cons cell
- increment register
- decrement register


http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm#function
https://lispcookbook.github.io/cl-cookbook/debugging.html#inspect-and-describe

https://stackoverflow.com/questions/21410233/why-is-sharp-quote-notation-unnecessary-in-clisp?rq=1
#' - will return the lexical refernece 
'  - will return the functional value (return value of the function).



Reader algorithm.
http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm



## Ideas.

https://stackoverflow.com/questions/267862/what-makes-lisp-macros-so-special
https://stackoverflow.com/questions/2571401/why-exactly-is-eval-evil
https://wiki.c2.com/?LispShowOffExamples
https://lisptips.com/
https://wiki.c2.com/?HowToSumFromOneToTenInLispOrScheme

## Explorations

Some HTTP libraries:
http://edicl.github.io/hunchentoot/
https://common-lisp.net/project/simple-http/user-guide.html
https://quickref.common-lisp.net/trivial-http.html

Crypto library
http://method-combination.net/lisp/ironclad/

A good overview of practical usage:
http://lispcookbook.github.io/cl-cookbook/

http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#class

### What's bad about Lisp?

http://www.findinglisp.com/blog/2005/12/reddit-and-lisp-psychosis.html
https://github.com/reddit-archive/reddit1.0

#### The stack traces

(base) ➜  Lisp ./1.lisp                                             

; file: /Users/liamz/Documents/Education/Lisp/1.lisp
; in: SETQ DOG
;     (SETQ DOG 'CAT)
; 
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::DOG
; 
; compilation unit finished
;   Undefined variable:
;     DOG
;   caught 1 WARNING condition
Unhandled UNDEFINED-FUNCTION in thread #<SB-THREAD:THREAD "main thread" RUNNING
                                          {1000508083}>:
  The function COMMON-LISP-USER::RESOLVE-HOST-IPADDR is undefined.

Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING {1000508083}>
0: ("undefined function" "www.lisp.org")
1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (RESOLVE-HOST-IPADDR "www.lisp.org") #<NULL-LEXENV>)
2: (EVAL-TLF (RESOLVE-HOST-IPADDR "www.lisp.org") 3 NIL)
3: ((LABELS SB-FASL::EVAL-FORM :IN SB-INT:LOAD-AS-SOURCE) (RESOLVE-HOST-IPADDR "www.lisp.org") 3)
4: ((LAMBDA (SB-KERNEL:FORM &KEY :CURRENT-INDEX &ALLOW-OTHER-KEYS) :IN SB-INT:LOAD-AS-SOURCE) (RESOLVE-HOST-IPADDR "www.lisp.org") :CURRENT-INDEX 3)
5: (SB-C::%DO-FORMS-FROM-INFO #<CLOSURE (LAMBDA (SB-KERNEL:FORM &KEY :CURRENT-INDEX &ALLOW-OTHER-KEYS) :IN SB-INT:LOAD-AS-SOURCE) {100154537B}> #<SB-C::SOURCE-INFO {1001545343}> SB-C::INPUT-ERROR-IN-LOAD)
6: (SB-INT:LOAD-AS-SOURCE #<SB-SYS:FD-STREAM for "file /Users/liamz/Documents/Education/Lisp/1.lisp" {1001536AA3}> :VERBOSE NIL :PRINT NIL :CONTEXT "loading")
7: ((FLET SB-FASL::THUNK :IN LOAD))
8: (SB-FASL::CALL-WITH-LOAD-BINDINGS #<CLOSURE (FLET SB-FASL::THUNK :IN LOAD) {B1A569B}> #<SB-SYS:FD-STREAM for "file /Users/liamz/Documents/Education/Lisp/1.lisp" {1001536AA3}>)
9: ((FLET SB-FASL::LOAD-STREAM :IN LOAD) #<SB-SYS:FD-STREAM for "file /Users/liamz/Documents/Education/Lisp/1.lisp" {1001536AA3}> NIL)
10: (LOAD #<SB-SYS:FD-STREAM for "file /Users/liamz/Documents/Education/Lisp/1.lisp" {1001536AA3}> :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :EXTERNAL-FORMAT :DEFAULT)
11: ((FLET SB-IMPL::LOAD-SCRIPT :IN SB-IMPL::PROCESS-SCRIPT) #<SB-SYS:FD-STREAM for "file /Users/liamz/Documents/Education/Lisp/1.lisp" {1001536AA3}>)
12: ((FLET SB-UNIX::BODY :IN SB-IMPL::PROCESS-SCRIPT))
13: ((FLET "WITHOUT-INTERRUPTS-BODY-2" :IN SB-IMPL::PROCESS-SCRIPT))
14: (SB-IMPL::PROCESS-SCRIPT "./1.lisp")
15: (SB-IMPL::TOPLEVEL-INIT)
16: ((FLET SB-UNIX::BODY :IN SAVE-LISP-AND-DIE))
17: ((FLET "WITHOUT-INTERRUPTS-BODY-14" :IN SAVE-LISP-AND-DIE))
18: ((LABELS SB-IMPL::RESTART-LISP :IN SAVE-LISP-AND-DIE))

unhandled condition in --disable-debugger mode, quitting


#### Misc
