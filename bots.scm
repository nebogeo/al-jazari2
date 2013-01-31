;; thinking machines

(load "list.scm")
(load "octree.scm")

(define (make-bot id pos code) (list id pos 0 code))
(define (bot-id b) (list-ref b 0))
(define (bot-pos b) (list-ref b 1))
(define (bot-clock b) (list-ref b 2))
(define (bot-code b) (list-ref b 3))
(define (bot-modify-pos b v) (list-replace b 1 v))
(define (bot-modify-clock b v) (list-replace b 2 v))
(define (bot-modify-code b v) (list-replace b 3 v))

(define (bot-test-code bot)
  (let ((dir (vadd
              (if (key-pressed "w") (vector 1 0 0) (vector 0 0 0))
              (if (key-pressed "s") (vector -1 0 0) (vector 0 0 0))
              (if (key-pressed "a") (vector 0 0 -1) (vector 0 0 0))
              (if (key-pressed "d") (vector 0 0 1) (vector 0 0 0)))))
    (bot-modify-pos bot (vadd (bot-pos bot) dir)))

  #;(bot-modify-pos
   bot
   (vadd 
    (list-ref
     (list (vector 1 0 0)
           (vector 0 0 1)
           (vector -1 0 0)
           (vector 0 0 -1))
     (modulo (bot-clock bot) 4))
    (bot-pos bot))))

(define (bot-run bot octree)
  (let ((bot (bot-modify-clock bot (+ 1 (bot-clock bot)))))
    ;; check gravity first
    (let ((here (octree-ref octree (bot-pos bot)))
          (under (octree-ref octree (vadd (vector 0 -1 0) (bot-pos bot)))))
      (if (not (eq? here 'e)) ;; 'in' a filled block, go up
          (bot-modify-pos bot (vadd (vector 0 1 0) (bot-pos bot)))
          (if (eq? under 'e) ;; nothing underneath, go down
              (bot-modify-pos bot (vadd (vector 0 -1 0) (bot-pos bot)))
              (bot-test-code bot)))))) ;; todo: run code
  
(define (make-bots l) (list l))
(define (bots-list bs) (list-ref bs 0)) 
(define (bots-modify-list bs v) (list-replace bs 0 v)) 

(define (bots-add-bot bs bot)
  (bots-modify-list 
   bs 
   (cons bot (bots-list bs))))

(define (bots-run bs octree)
  (bots-modify-list
   bs
   (map
    (lambda (bot)
      (bot-run bot octree))
    (bots-list bs))))

