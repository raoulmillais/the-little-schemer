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
(atom? 'atom) ; => true
(atom? (quote turkey)) ; => true
(atom? 1492) ; => true
(atom? (quote u)) ; => true
(atom? (quote *abc$)) ; => true

; lists
(atom? '('atom)) ; => false
(atom? '('atom 'turkey 'or)) ; => false
(atom? '('('atom 'turkey) 'or)) ; => false

; S-expressions - all atoms and lists are S-expressions
(atom?  'xyz) ; => true
(atom? '('x 'y 'z)) ; => false
(atom? '('(x y) 'z)) ; => false
(atom? '()) ; => false
(atom? '('() '() '())) ; => false

; car
(car '('a 'b 'c)) ; => 'a
(car '('('a 'b) 'c)) ; => '('a 'b)

; cdr
(cdr '('a 'b 'c)) ; => '('b 'c)
(cdr '('('a 'b 'c) 'x 'y 'z)) ; => '('x 'y 'z)
(cdr '('hamburger)) ; => '()

; cons
(cons 'peanut '('butter 'and 'jelly)) ; => '(peanut 'butter 'and 'jelly)
(cons '('(help) 'this) '(is 'very '('('hard) 'to 'learn)))
; => '('('(help) 'this) 'is 'very '('('hard) 'to 'learn))
(cons '('a 'b '('c)) '()) ; => '('('a 'b '('c)))
(cons 'a '()) ; => '('a)

; null?
(null? (quote ())) ; => true
(null? '()) ; => true
(null? '('a 'b 'c)) ; => false
(null? 'a) ; => false (technically no answer as null only applies to lists

; eq?
(eq? 'a 'a) ; => true
(eq? 'a 'b) ; => false
(eq? '('a) '('a)) ; => false
(eq? (car '('Mary 'had 'a 'little 'lamb)) 'Mary)
; => true according to the book but false in guile.
