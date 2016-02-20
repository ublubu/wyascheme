(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id obj) obj)
(define (flip func) (lambda (a b) (func b a)))

(define (curry func arg1) (lambda arg (apply func (cons arg1 arg))))
(define (compose f g) (lambda (arg) (f (g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))
