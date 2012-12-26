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

; numbers
(atom? 14) ; => true
(atom? -13) ; => true
(number? 3.14159) ; => true
(add1 67) ; => 68
(sub1 5) ; => 4
(zero? 0) ; => true
(zero? 1492) ; => false

; o+ - add m to n
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

; o- - subtract m from n
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; addtup - adds together all the numbers in tuple `tup`
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

; x - multiply n by m
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (x n (sub1 m)))))))

; tup+ - returns a new tuple containing the sum of the each number in `tup1`
; and its corresponding number in `tup2`
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

; o> - determines whether `n` is greater than `m`
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

; o< - determines whether `n` is less than `m`
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

; o= - determines whether `n` is equal to `m`
(define o=
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (o= (sub1 n) (sub1 m))))))

; o=2 - determines whether `n` is equal to `m`
(define o=2
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

; pow - computes the power of `n` to `m`
(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (pow n (sub1 m)))))))

; divide - computes the integer division of `m` in `n`
(define divide
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (divide (- n m) m))))))

; length - counts the number of S-expressions in the list `l`
(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

; pick - picks the `n`th element from the list `l`
(define pick
  (lambda (n l)
    (cond
      ((zero? (sub1 n)) (car l))
      (else (pick (sub1 n) (cdr l))))))

; rempick - returns a new list containg all the S-expressions from list `l`
; with the `n`th S-expression removed
(define rempick
  (lambda (n l)
    (cond
      ((zero? (sub1 n)) (cdr l))
      (else (cons (car l) (rempick (sub1 n) (cdr l)))))))

; no-nums - returns a new list with all the numbers removed from list `l`
(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else (cons (car l) (no-nums (cdr l)))))))

; all-nums - returns a new list with only the numbers from list `l`
(define all-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (cons (car l) (all-nums (cdr l))))
      (else (all-nums (cdr l))))))

; eqan - test whether two atoms are equal numbers or otherwise
(define eqan
  (lambda (a b)
    (cond
      ((and (number? a) (number? b))
       (= a b))
      ((or (number? a) (number? b))
       #f)
      (else (eq? a) (eq? b)))))

; occur - computes the number of times the atom `a` appears in the list `l`
(define occur
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((eq? (car l) a) (add1 (occur a (cdr l))))
      (else (occur a (cdr l))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))
