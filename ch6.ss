(use-modules (debugging assert))

; atom? - check whether `x` is an atom or not
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))

;sub1 - subtract 1 from `x`
(define sub1
  (lambda (x)
    (- x 1)))

; pow - computes the power of `n` to `m`
(define pow
	(lambda (n m)
		(cond
			((zero? m) 1)
			(else (* n (pow n (sub1 m)))))))

(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			((equal? (car (cdr aexp)) (quote +))
				(and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((equal? (car (cdr aexp)) (quote x))
				(and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
			((equal? (car (cdr aexp)) (quote ^))
				(and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(let ((addition '(1 '+ 2)))
	(assert
		(numbered? addition)))

(let ((multiplication '(3 'x 2)))
	(assert
		(numbered? multiplication)))

(let ((exponentiation '(3 '^ 2)))
	(assert
		(numbered? exponentiation)))

(define value
	(lambda (aexp)
		(cond
			((atom? aexp) aexp)
			((eq? (car (cdr aexp)) (quote +))
			 (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
			((eq? (car (cdr aexp)) (quote x))
						(* (value (car aexp)) (value (car (cdr (cdr aexp))))))
			(else
				(pow (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

(assert
	(equal? (value 4) 4))

(assert
	(equal? (value '(1 + 3)) 4))

(assert
	(equal? (value '(2 ^ 3)) 8))

(assert
	(equal? (value '(1 + (3 ^ 4))) 82))

(define first-sub-exp
	(lambda (exp)
		(car (cdr exp))))

(define second-sub-exp
	(lambda (exp)
		(car (cdr (cdr exp)))))


