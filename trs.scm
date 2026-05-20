(use-modules (tls-primitives))

;; Starts with these, but then revises them

;; (define is-first?
;;   (lambda (a lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else (eq? a (car lat))))))

;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else
;;         (or
;;           (is-first? (car lat) (cdr lat))
;;           (two-in-a-row? (cdr lat)))))))

;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else
;;         (is-first-b? (car lat) (cdr lat))))))

;; (define is-first-b?
;;   (lambda (a lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else
;;         (or
;;           (eq? a (car lat))
;;           (two-in-a-row? lat))))))

;; (define two-in-a-row-b?
;;   (lambda (preceding lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else
;;         (or
;;           (eq? preceding (car lat))
;;           (two-in-a-row-b? (car lat) (cdr lat)))))))

;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else
;;           (two-in-a-row-b? (car lat) (cdr lat))))))

;; (two-in-a-row? '("dear" "dear" "i" "am" "sleepy"))

;; Takes a tuple and creates a new tuple where each element is the sum of the
;; preceding element in the original tuple.
;; I disliked this exercise because it wasn't obvious you needed to build
;; the majority of the function into the helper function. I thought it was
;; going to be more like the previous example where the responsibilities were
;; more split up.
;; (define sum-of-prefixes
;;   (lambda (tup)
;;     (sum-of-prefixes-b 0 tup)))

;; (define sum-of-prefixes-b
;;   (lambda (sonssf tup)
;;     (cond
;;       ((null? tup) '())
;;       (else
;;         (cons
;;           (+ sonssf (car tup))
;;           (sum-of-prefixes-b
;;             (+ sonssf (car tup))
;;             (cdr tup)))))))

;; Anyway, here's my version. I think it's easier to comprehend, though
;; the additional null check is a bit more awkward.
;; (define sum-of-prefixes
;;   (lambda (tup)
;;     (cond
;;       ((null? tup) '())
;;       (else
;;         (running-total (car tup) tup)))))

;; (define running-total
;;   (lambda (total remaining)
;;     (cond
;;       ((null? remaining) '())
;;       (else
;;         (cons
;;           total
;;           (running-total
;;             (+ total (car remaining))
;;             (cdr remaining)))))))


;; (sum-of-prefixes '(1 1 1 1 1))

(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))

;; (pick 2 '("foo" "bar" "baz"))

;; Some monstrous function they cooked up.
(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
        (cons
          (pick (car tup)
            (cons
              (car tup)
              rev-pre))
          (scramble-b (cdr tup)
            (cons (car tup) rev-pre)))))))

(define multirember
  (lambda (a lat-outer)
    (letrec
      ((mr (lambda (lat)
        (cond 
	  ((null? lat) '())
	  ((eq? a (car lat)) (mr (cdr lat)))
	  (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat-outer))))

(display (multirember "foo" '("foo" "bar" "baz")))
(newline)
