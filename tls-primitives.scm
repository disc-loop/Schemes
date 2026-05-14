(define-module (tls-primitives)
  #:export (atom? lat? add1 sub1))

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (λ (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define add1
  (λ (n)
    (+ n 1)))

(define sub1
  (λ (n)
    (- n 1)))
