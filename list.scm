;; al jazari two (c) 2013 dave griffiths gpl v3

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (in-list? a l)
  (cond 
   ((null? l) #f)
   ((eq? (car l) a) #t)
   (else (in-list? a (cdr l)))))

(define (list-ref-safe l n)
  (list-ref l (modulo n (length l))))
