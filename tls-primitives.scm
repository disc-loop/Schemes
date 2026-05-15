(define-module (tls-primitives)
  #:export (atom? lat? add1 sub1 print))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))


;; Not in the book
(define print
  (lambda (x)
    (display x)
    (newline)))
