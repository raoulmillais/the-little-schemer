(use-modules (debugging assert))

; atom? - check whether `x` is an atom or not
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; member? - determine whether `a` is a member of list `l`
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else
        (cond
          ((equal? (car l) a) #t)
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

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else  #f))))

(assert
  (subset?
   '(5 chicken wings)
   '(5 hamburgers 2 pieces fried chicken and light duckling wings)))

(assert
  (not (subset?
   '(4 pounds of horseradish)
   '(four pounds chicken 5 ounces horseradish))))

(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset2? (cdr set1) set2))))))

(assert
  (subset2?
   '(5 chicken wings)
   '(5 hamburgers 2 pieces fried chicken and light duckling wings)))

(assert
  (not (subset2?
   '(4 pounds of horseradish)
   '(four pounds chicken 5 ounces horseradish))))

(define eqset?
  (lambda (set1 set2)
    (cond
      ((subset? set1 set2) (subset? set2 set2))
      (else #f))))

(assert
  (eqset? '(6 large chickens with wings) '(6 large chickens with wings)))

(define eqset2?
  (lambda (set1 set2)
    (cond
      (else (and (subset? set1 set2) (subset? set2 set2))))))

(assert
  (eqset2? '(6 large chickens with wings) '(6 large chickens with wings)))

(define eqset3?
  (lambda (set1 set2)
      (and (subset? set1 set2) (subset? set2 set2))))

(assert
  (eqset3? '(6 large chickens with wings) '(6 large chickens with wings)))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
        (cond
           ((member? (car set1) set2) #t)
           (else (intersect? (cdr set1) set2)))))))

(assert
  (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)))

(define intersect2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect2? (cdr set1) set2)))))

(assert
  (intersect2? '(stewed tomatoes and macaroni) '(macaroni and cheese)))

(define intersect3?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((or (member? (car set1) set2)) (intersect3? (cdr set1) set2)))))

(assert
  (intersect3? '(stewed tomatoes and macaroni) '(macaroni and cheese)))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(assert
  (equal?
    (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
    '(and macaroni)))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2) )
      (else (cons (car set1) (union (cdr set1) set2))))))

(assert
  (equal?
    (union
      '(stewed tomatoes and macaroni casserole)
      '(macaroni and cheese))
    '(stewed tomatoes casserole macaroni and cheese)))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(assert
  (equal?
    (difference '(apples blueberries pears) '(blueberries peaches oranges))
    '(apples pears)))

(define intersectall
  (lambda (set)
    (cond
      ((null? (cdr set)) (car set))
      (else (intersect (car set) (intersectall (cdr set)))))))

(assert
  (equal?
    (intersectall
      '((a b c) (c a d e) (e f g h a b)))
    '(a)))

(assert
  (equal?
    (intersectall
      '((6 pears and)
        (3 peaches and 6 peppers)
        (8 pears and 6 plums)
        (and 6 prunes with some apples)))
    '(6 and)))

(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t)
      (else #f))))

(assert
  (a-pair? '(3 4)))

(assert
  (a-pair? '((2) (pair))))

(assert
  (a-pair? '(full '(house))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; TODO - Implement rel?
(assert
 (not (rel? '(apples peaches pumpkin pie))))

(assert
  (not (rel? '((apples peaches) (pumpkin pie) (apples peaches)))))

(assert
  (rel? '((apples peaches) (pumpkin pie)))))
