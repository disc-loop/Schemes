#lang racket

(provide josephus-survivor)

; Add people to circle
(define addToCircle
  lambda (n)
    (cond ((zero? n) '())
          (else (cons n addToCircle))))

; Remove people and return last person (- people 1)
(define removePeople
  lambda (s l)
    (cond ((equal? (size l) 1) l)
          (else (removePeople s ()))))

; WIP
(define (josephus-survivor n k)
  )
