(define (map fn ls)
  (if (null? ls)
      nil
      (cons (fn (car ls)) (map fn (cdr ls)))))
(define (even? num) (= (% num 2) 0))