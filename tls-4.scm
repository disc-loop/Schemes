(use-modules (tls-primitives))

;; If null, return the null list as we need to build a new list.
;; Check if first elem is an atom.
;;   If it is, compare to a.
;;     If it is a, recurse on the remainder of the list (thus skipping the elem just checked).
;;     Else, cons it onto the natural recursion of the list.
;; Else, the first elem of the list must be a list.
;;   Cons the natural recursion of the sub-list with the natural recursion of the remaining list.
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(print (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))
