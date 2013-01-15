(use-modules (debugging assert))

; member? - determine whether `a` is a member of list `l`
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else
        (cond
          ((eq? (car l) a) #t)
          (else (member? a (cdr l))))))))

; multirember - remove each occurrence of `a` from the list `l`
(define multirember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) (multirember a (cdr l)))
      (else (cons (car l)
                  (multirember a (cdr l)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(assert
  (not (set? '(apple peaches apple plum))))

(assert
  (set? '(apple peaches pears plum)))

(assert
  (not (set? '(apple 3 pear 4 9 apple 3 4))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(assert
  (equal?
    (makeset '(apple peach pear peach plum apple lemon peach))
    '(pear plum apple lemon peach)))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons
        (car lat)
        (makeset2 (multirember (car lat) (cdr lat))))))))

(assert
  (equal?
    (makeset2 '(apple peach pear peach plum apple lemon peach))
    '(apple peach pear plum lemon)))
