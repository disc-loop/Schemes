(use-modules (tls-primitives))

; This chapter focuses on the halting problem, the nature of recursion, and the
; Y combinator.

(define pick
  (lambda (n lat)
	  (cond 
		  ((zero? (- n 1)) (car lat))
			(else (pick (- n 1) (cdr lat))))))

; The book says this fn is weird because it uses "unnatural" recursion, i.e. the
; recursive call operates on the same list, not a subset.
(define keep-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn) (keep-looking a (pick sorn lat) lat))
			(else (eq? sorn a)))))

; This is called a "partial" function as not all inputs have an output.
(define looking
  (lambda (a lat)
	  (keep-looking a (pick 1 lat) lat)))

(print (looking 'caviar '(6 2 4 caviar 5 7 3)))

; The most partial function with the most unnatural recursion possible, as none
; of its inputs map to any values and no subsets of the list are ever 
; evaluated.
(define eternity
	(lambda (x)
		(eternity x)))

(define build
  (lambda (s1 s2)
		(cons s1 (cons s2 '()))))

(define first
  (lambda (l)
		(car l)))

(define second
	(lambda (l)
		(car (cdr l))))

; I don't think it's worth trying to come up with this one on your own. They
; only give you two examples of its application prior to asking you to write
; it without specifying what it should do! It's not even the main point of 
; the sections anyway.
(define shift
	(lambda (pair)
		(build (first (first pair))
			(build (second (first pair))
				(second pair)))))

; This is the applicative-order Y combinator. I did some reading on why it's
; called that and turns out this is a modified version of the normal-order
; Y combinator. The reason there's two is because the normal order variant
; will never terminate in an applicative-order (eager) language like Scheme,
; but it works fine in a normal-order (lazy) language like Haskell. Here is
; the normal order version:
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

; This is the applicative-order version, often called the Z combinator:
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

; Interestingly, that's different to how they define it in the book:
(define Y-tls
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (v) ((x x) v)))))))
