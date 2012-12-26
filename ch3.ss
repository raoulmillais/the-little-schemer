; rember - remove the first occurrence of `a` from the list `l`
(define rember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember a (cdr l)))))))

; firsts - returns a new list containing the first S-expression of each sublist
; in list `l`
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; insertR - returns a new list with `new` inserted after the first occurrence
; of `old` in list `l`
(define insertR
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons old (cons new (cdr l))))
      (else (cons (car l) (insertR new old (cdr l)))))))

; subst - returns a new list with the first occurrence of `old` in list `l`
; replaced by `new`
(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l) (subst  new old (cdr l)))))))

; subst2 - returns a new list with the first occurrence of `o1` or `o2` in list
; `l` replaced by `new`
(define subst2
  (lambda (new o1 o2 l)
    (cond
      ((null? l) '())
      ((or (eq? (car l) o1) (eq? (car l) o2)) (cons new (cdr l)))
      (else (cons (car l) (subst2  new o1 o2 (cdr l)))))))

; multirember - remove each occurrence of `a` from the list `l`
(define multirember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) (multirember a (cdr l)))
      (else (cons (car l)
                  (multirember a (cdr l)))))))

; multiinsertR - returns a new list with `new` inserted after each occurrence
; of `old` in list `l`
(define multiinsertR
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons old (cons new (multiinsertR new old (cdr l)))))
      (else (cons (car l) (multiinsertR new old (cdr l)))))))

; multiinsertL - returns a new list with `new` inserted before each occurrence
; of `old` in list `l`
(define multiinsertL
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (cons old (multiinsertL new old (cdr l)))))
      (else (cons (car l) (multiinsertL new old (cdr l)))))))

; multisubst - returns a new list with each occurrence of `old` in list `l`
; replaced by `new`
(define multisubst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (multisubst new old (cdr l))))
      (else (cons (car l) (multisubst  new old (cdr l)))))))

