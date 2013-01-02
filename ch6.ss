(use-modules (debugging assert))

; atom? - check whether `x` is an atom or not
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(let ((garbage '(3 'sausage 2)))
  (assert
    (not (numbered? garbage))))
