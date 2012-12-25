; lat? - determine wheter `l` is a list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; member? - determine whether `a` is a member of list `l`
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else 
        (cond
          ((eq? (car l) a) #t)
          (else (member? a (cdr l))))))))
