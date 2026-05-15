(use-modules (tls-primitives))

(define insertR
  (lambda (a b lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
        (cons a (cons b (cdr lat))))
      (else
	(cons (car lat) (insertR a b (cdr lat)))))))

;;(print (insertR 'a 'b '(a c)))

(define subst
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else
	(cons (car lat) (subst old new (cdr lat)))))))

;;(print (subst 'jelly 'honey '(peanut butter and jelly is much better than peanut butter and jelly)))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
	(cons (car lat) (multirember a (cdr lat)))))))

(print (multirember 'bad '(good, bad bad bad bad very good bad)))
