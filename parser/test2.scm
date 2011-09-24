(define list (lambda args args))
(define (map fn ls)
  (if (null? ls)
      nil
      (cons (fn (car ls)) (map fn (cdr ls)))))
(define str "string")
(define x 1)
(define x 2)
(lambda (x) x)
(lambda (y) (+ x y));; Whoah, is comment! is crazy!
(lambda (a) (lambda (b) (+ a b x)))
(define (a b) (+ b x));; It's a new function!


