; Understanding why some of these examples were not behaving as anticipated
; proved quite tricky.  The lack of understanding of quoting and how to build
; literal lists caused me quite a bit of confusion.

; (list 'a 'b 'c) => ('a 'b 'c)
; '('a 'b 'c) => (quote ('a 'b 'c)

; Note that the atoms must be quoted or the interpreter will try to evaluate
; them as variables.  A list must be created using the `list` function.  The
; REPL will interpret the first S-expression as the symbol representing a
; function that must be called.

; Use the assert function from the debugging module in guile-lib so that we can
; verify the behaviour of our functions
(use-modules (debugging assert))

; atom? - check whether `x` is an atom or not
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; add1 - add 1 to `x`
(define add1
  (lambda (x)
    (+ x 1)))

;sub1 - subtract 1 from `x`
(define sub1
  (lambda (x)
    (- x 1)))

; atoms
(assert
  (atom? 'atom)) ; => true
(assert
  (atom? (quote turkey))) ; => true
(assert
  (atom? 1492)) ; => true
(assert
  (atom? (quote u))) ; => true
(assert
  (atom? (quote *abc$))) ; => true

; lists
(assert
  (not
    (atom? (list 'atom)))) ; => false
(assert
  (not
    (atom? (list 'atom 'turkey 'or)))) ; => false
(assert
  (not
    (atom? (list (list 'atom 'turkey) 'or)))) ; => false

; S-expressions - all atoms and lists are S-expressions
(assert
  (atom?  'xyz)) ; => true
(assert
  (not
    (atom? (list 'x 'y 'z)))) ; => false
(assert
  (not
    (atom? (list (list 'x 'y) 'z)))) ; => false
(assert
  (not
    (atom? '()))) ; => false
(assert
  (not
    (atom? (list '() '() '())))) ; => false

; car
(assert
  (eq?
    (car (list 'a 'b 'c))
    'a)) ; => 'a

; We don't know how to do a deep equal yet so we must bind the list ('a 'b)
; to the variable `letters` to check that the `car` of our list is equal
; to it
(let
  ((letters (list 'a 'b)))
  (assert
    (eq?
      (car (list letters 'c))
      letters))) ; => ('a 'b)

; cdr
(cdr (list 'a 'b 'c)) ; => ('b 'c)
(cdr (list (list 'a 'b 'c) 'x 'y 'z)) ; => ('x 'y 'z)
(assert
  (eq?
    (cdr (list 'hamburger))
    '())) ; => '()

; cons
(cons 'peanut (list 'butter 'and 'jelly)) ; => (peanut 'butter 'and 'jelly)
(cons (list (list 'help) 'this) (list 'is 'very (list (list 'hard) 'to 'learn)))
; => ((('help) 'this) 'is 'very (('hard) 'to 'learn))
(cons '('a 'b '('c)) '()) ; => '('('a 'b '('c)))
(cons 'a '()) ; => '('a)

; null?
(assert
  (null? (quote ()))) ; => true
(assert
  (null? '())) ; => true
(assert
  (not
    (null? (list 'a 'b 'c)))) ; => false
(assert
  (not
    (null? 'a))) ; => false (technically no answer as null only applies to lists

; eq?
(assert
  (eq? 'a 'a)) ; => true
(assert
  (not
    (eq? 'a 'b))) ; => false
(assert
  (not
    (eq? (list 'a) (list 'a)))) ; => false
(assert
  (eq? (car (list 'Mary 'had 'a 'little 'lamb)) 'Mary)) ; => true
