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

;(print (looking 'caviar '(6 2 4 caviar 5 7 3)))

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
; the normal order version (I think this particular example only works for
; functions with 0 args though):
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

; This is the applicative-order version, also called the Z combinator (this
; example takes a function that accepts 1 arg, v):
(define Z
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

; I found this difficult to understand. I think part of difficulty comes from
; Scheme's minimalism - it's hard to know what each variable represents because
; there's no type identifiers. Without that information, you have to infer what 
; the function requires for it to work when applied. Relatedly, the other issue 
; is that the Y/Z combinator will not work for any random function but instead 
; requires a function of a special form. Thankfully, it's not hard to convert a 
; regular recursive function into this form, but it is not obvious that it's 
; required by Z from the definition alone.

; Here's a version of the Z combinator with less cryptic variable names, as well
; as an example of factorial in the special form:
(define EZ
	(lambda (special)
		((lambda (again) (again again))
		 (lambda (this-again)
		   (special (lambda (arg) ((this-again this-again) arg)))))))

(define special-factorial
	(lambda (factorial)
	  (lambda (n)
	  	(if (<= n 1)
	  			1
	  			(* n (factorial (- n 1)))))))

(print ((EZ special-factorial) 3))

; And here's a worked example. First, let's look at the body of EZ:
(lambda (special)
  ((lambda (again) (again again))
   (lambda (this-again) (special (lambda (arg) ((this-again this-again) arg))))))

; Let's apply the first function to the second. That becomes this:
(lambda (special)
  ((lambda (this-again) (special (lambda (arg) ((this-again this-again) arg))))
   (lambda (this-again) (special (lambda (arg) ((this-again this-again) arg))))))

; From there, you can see that the second function is fed into the first one
; and that reproduces the exact same structure. However, the loop gets
; suspended in the lambda expression passed to special. It's only once that
; function is called within special that the loop continues.
