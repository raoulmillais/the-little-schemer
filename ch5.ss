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

; eqan? - test whether two atoms are equal numbers or otherwise
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b))
       (= a b))
      ((or (number? a) (number? b))
       #f)
      (else (eq? a b)))))

(let ((a 'a) (b 'b))
  (assert
    (eqan? 1 1)
    (eqan? a a)
    (not (eqan? 1 2))
    (not (eqan? a 1))
    (not (eqan? 1 a))
    (not (eqan? a b))))

; rember* - remove all instances from list `l` and recusively through all
; sublists
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(let ((original
        (list (list 'coffee) 'cup (list (list 'tea) 'cup)
         (list 'and (list 'hick)) 'cup))
      (expected
       (list (list 'coffee) (list (list 'tea)) (list 'and (list 'hick)))))
  (assert
    (equal?
      (rember* 'cup original)
      expected)))

(let ((original
        (list (list (list 'tomato 'sauce)) (list (list 'bean) 'sauce)
         (list 'and (list (list 'flying)) 'sauce)))
      (expected
       (list (list (list 'tomato)) (list (list 'bean))
        (list 'and (list (list 'flying)))) ))
  (assert
    (equal?
      (rember* 'sauce original)
      expected)))

; insertR* - insert an instance of `new` before all instances of `old`
; list `l` and recusively through all sublists
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(let ((original
        (list 'how 'much (list 'wood) 'could
          (list 'a (list 'wood) 'chuck)
            (list 'chuck)
               (list 'if (list 'a) (list 'wood 'chuck)
                'could 'chuck 'wood)))
      (expected
          (list 'how 'much (list 'wood) 'could
            (list 'a (list 'wood) 'chuck 'roast)
              (list 'chuck 'roast)
                (list 'if (list 'a) (list 'wood 'chuck 'roast)
                 'could 'chuck 'roast 'wood))))
  (assert
    (equal?
     (insertR* 'roast 'chuck original)
     expected)))

; occur* - count all occurrences of `a` in list `l` ans recursively through all
; sublists
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eqan? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(let ((ice-cream
      (list (list 'banana)
        (list 'split (list (list (list (list 'banana 'ice)))
          (list 'cream (list 'banana))
            'sherbet))
          (list 'banana)
          (list 'bread)
          (list 'banana 'brandy))))
  (assert
    (= (occur* 'banana ice-cream) 5)))

; subst* - replace all occurrences of `old` with `new` in list `l` and
; recursively through all sublists
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(let ((original
        (list (list 'banana)
            (list 'split (list (list (list (list 'banana 'ice)))
            (list 'cream (list 'banana)) 'sherbet))
          (list 'banana)
          (list 'bread)
          (list 'banana 'brandy)))
      (expected
         (list (list 'orange)
            (list 'split (list (list (list (list 'orange 'ice)))
            (list 'cream (list 'orange)) 'sherbet))
          (list 'orange)
          (list 'bread)
          (list 'orange 'brandy))))
  (assert
    (equal?
      (subst* 'orange 'banana original)
      expected)))

; insertL* - insert `new` before all occurrences of `old` in list `l` and
; recursively through all sublists
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(let ((original
            (list (list 'how 'much (list 'wood)) 'could
              (list (list 'a (list 'wood) 'chuck))
              (list (list (list 'chuck)))
              (list 'if (list 'a) (list (list 'wood 'chuck)))
                'could 'chuck 'wood))
      (expected
            (list (list 'how 'much (list 'wood)) 'could
              (list (list 'a (list 'wood) 'pecker 'chuck))
              (list (list (list 'pecker 'chuck)))
              (list 'if (list 'a) (list (list 'wood 'pecker 'chuck)))
                'could 'pecker 'chuck 'wood)))
  (assert
    (equal?
      (insertL* 'pecker 'chuck original)
      expected)))

; member* - searches list `l` and recursively through all sublists for an
; occurrence of `a`
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(let ((fish-n-chips
        (list
          (list 'potato)
          (list 'chips
            (list (list 'with) 'fish)
            (list 'chips)))))
  (assert
    (member* 'chips fish-n-chips)))

; leftmost - searches list `l` and recursively through sublists for the first
; atom
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(let ((fish-n-chips
        (list
          (list 'potato)
          (list 'chips
            (list (list 'with) 'fish)
            (list 'chips)))))
  (assert
    (eq?
      (leftmost fish-n-chips)
      'potato)))

; eqlist? - determines whether the contents of two lists are the same comparing
; recursively through all sublists
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
        #t)
      ((or (null? l1) (null? l2))
        #f)
      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eqan? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2)))
        #f)
      (else
        (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(assert
  (not
    (eqlist?
      (list 'strawberry 'ice 'cream)
      (list 'strawberry 'cream 'ice))))

(assert
  (not
    (eqlist?
      (list 'banana (list (list 'split)))
      (list (list 'banana) (list 'split)))))

; equal? - determines whether two S-expressions are the same recursively through
; all child S-expressions
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2) (eqan? s1 s2)))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist2? s1 s2)))))


; eqlist2? - determines whether the contents of two lists are the same comparing
; recursively through all sublists
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
        #t)
      ((or (null? l1) (null? l2))
        #f)
      (else
        (and (equal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))

; rember - remove the first occurrence of the S-expression `s` from the list
; `l` non-starred version because it only recurs on the `cdr` of `l` by using
; `equal?`
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l) (rember s (cdr l)))))))))
