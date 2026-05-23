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
; (define rember-f
;   (lambda (same? a l)
;     (cond
;       ((null? l) '())
;       ((same? (car l) a) (cdr l))
;       (else (cons (car l) (rember-f same? a (cdr l)))))))

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

; The whole section on collectors / continuations is quite confusing. Even now,
; I find it difficult to understand multirember&co. I think it's because of the
; closures and backtracking. It's hard to keep track of what col slowly becomes
; as it builds up through the recursion and this makes it difficult to step 
; through once it hits the base case.
;
; For instance, here's how it looks with the small example they give:
; 1. (eq? 'and 'tuna) = #f, so recurse with col as:
;    (lambda (newlat seen) (a-friend (cons 'and newlat) seen))
;
; 2. (eq? 'tuna 'tuna) = #t, so recurse with col as:
;    (lambda (newlat seen)
;      ((lambda (newlat seen) (a-friend (cons 'and newlat) seen))
;        newlat
;        (cons 'tuna seen)))
;
; 3. Finally, (null? lat) = #t, so we evaluate (col '() '()), which is this (I think):
;    ((lambda (newlat seen)
;       ((lambda (newlat seen) (a-friend (cons 'and newlat) seen))
;         newlat
;         (cons 'tuna seen)))
;           '()
;           '())
;
; 3. (cont.) stepping through:
;    ((lambda (newlat seen) (a-friend (cons 'and newlat) seen))
;      '()
;      (cons 'tuna '())))
;
; 3. (cont.) again:
;    (a-friend (cons 'and '()) '(tuna))
;
; 3. (cont.) finally:
;    #f
;
; Still, I think I get the general idea, it's just difficult to conceptualise.
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
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

; In the context of the example above, this could probably be renamed to
; none-removed?
(define a-friend
  (lambda (x y)
    (null? y)))

(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

; Remove all uneven numbers from the list.
; Building a list, so base case is null check that returns empty list on null.
; The list is either null or not null.
; The first elem of the list is either atomic or a list.
; If the first elem is atomic, check if even. If it is, construct a new list
; from it and the natural recursion on the remainder of the list.
; If the first element isn't even, return the natural recursion on the remander of the list.
; If the first elem is a list, construct a new list from the recursion using that list with
; the recursion of the remainder of the list.
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(print (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

; The next function they work through is just too much to write out.
; Anyway, I think they've made their point. You can "collect" values
; in the body of functions as you recurse. Then, once you reach the
; base case, you apply the supplied function to the result of the
; application of all the collected functions. 
; 
; There seem to be some big drawbacks to this approach, namely 1)
; generating a massive call stack, and 2) difficulty in understanding
; the final application of those functions. I think a better approach
; would be to just define additional params for the main function which
; allow for the collected values to be passed throughout the recursion.
