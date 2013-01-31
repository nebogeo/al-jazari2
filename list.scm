;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))
