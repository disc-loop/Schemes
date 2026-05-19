(use-modules (tls-primitives))

; Reusing some functions from the previous chapters
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (= a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2) #f))
      (else
        (and
          (equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))

; Implement rember-f:
; Three args: an equality function, the member to remove, and the list to remove it from.
; Building a list with recursion.
; If list is null, return null list
; If first elem in the list matches the member to remove: return the remainder of the list.
; Else, return a new list constructed from the first element and the natural recursion of rember-f.
(define rember-f
  (lambda (same? a l)
    (cond
      ((null? l) '())
      ((same? (car l) a) (cdr l))
      (else (cons (car l) (rember-f same? a (cdr l)))))))

; (print (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))))

(define eq?-c
  (lambda (x)
    (lambda (y)
      (eq? x y))))

(define rember-f
  (lambda (same?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((same? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f same?) a (cdr l))))))))

; (print ((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? l) (col '() '()))
      ((eq? (car lat) a)
       (multirember&co
         a
         (cdr lat)
         (lambda (newlat seen)
           (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co
          a
          (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(print (multirember&co 'tuna '(tuna) a-friend))
