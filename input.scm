;; al jazari two (c) 2013 dave griffiths gpl v3

(load "list.scm")

(define (make-input) (list (list) (list)))
(define (input-keys i) (list-ref i 0))
(define (input-modify-keys i v) (list-replace i 0 v))
(define (input-keys-down i) (list-ref i 1))
(define (input-modify-keys-down i v) (list-replace i 1 v))

(define (input-update i)
  ;; get keys pressed since last update
  (input-modify-keys 
   (input-modify-keys-down
    i
    (filter
     (lambda (key)
       (not (in-list? key (input-keys i))))
     (keys-down)))
   (keys-down)))

(define (input-key? i k)
  (in-list? (string-ref k 0) (input-keys-down i)))
