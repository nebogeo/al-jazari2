;; thinking machines

(load "list.scm")
(load "octree.scm")
(load "input.scm")
(load "scheme-bricks.scm")

(define bot-cycle-time 0.5)

(define (make-bot id pos brick-id) (list id pos 0 0 brick-id 'none (+ (time) (random))))
(define (bot-id b) (list-ref b 0))
(define (bot-pos b) (list-ref b 1))
(define (bot-dir b) (list-ref b 2))
(define (bot-clock b) (list-ref b 3))
(define (bot-brick-id b) (list-ref b 4))
(define (bot-action b) (list-ref b 5))
(define (bot-modify-pos b v) (list-replace b 1 v))
(define (bot-modify-dir b v) (list-replace b 2 v))
(define (bot-modify-clock b v) (list-replace b 3 v))
(define (bot-modify-action b v) (list-replace b 5 v))
(define (bot-time b) (list-ref b 6))
(define (bot-modify-time b v) (list-replace b 6 v))

(define (bot-run-code bot octree input bricks)
  ;; check gravity first
  (let ((here (octree-ref octree (bot-pos bot)))
        (under (octree-ref octree (vadd (vector 0 -1 0) (bot-pos bot)))))
    (if (not (eq? here 'e)) ;; 'in' a filled block, go up
        (bot-modify-pos bot (vadd (vector 0 1 0) (bot-pos bot)))
        (if (eq? under 'e) ;; nothing underneath, go down
            (bot-modify-pos bot (vadd (vector 0 -1 0) (bot-pos bot)))
            ;; if it't time
            (if (< (bot-time bot) (time)) 
                (bot-modify-clock 
                 (bot-modify-time
                  ;; run the code
                  (with-handlers ([(lambda (x) #t) 
                                   (lambda (n)
                                     (broadcast 1 (exn-message n))
                                     bot)])
                                 (apply 
                                  (eval (brick->sexpr (bricks-search bricks (bot-brick-id bot))))
                                  (list bot input)))
                  (+ (bot-time bot) bot-cycle-time))
                 (+ 1 (bot-clock bot)))
                bot)))))

(define (bot-in-front bot)
  (let ((d (modulo (bot-dir bot) 4)))
    (vadd
     (bot-pos bot)
     (if (eq? d 0) (vector 0 0 -1) (vector 0 0 0))
     (if (eq? d 1) (vector -1 0 0) (vector 0 0 0))
     (if (eq? d 2) (vector 0 0 1) (vector 0 0 0))
     (if (eq? d 3) (vector 1 0 0) (vector 0 0 0)))))

(define (bot-behind bot)
  (let ((d (modulo (bot-dir bot) 4)))
    (vadd
     (bot-pos bot)
     (if (eq? d 0) (vector 0 0 1) (vector 0 0 0))
     (if (eq? d 1) (vector 1 0 0) (vector 0 0 0))
     (if (eq? d 2) (vector 0 0 -1) (vector 0 0 0))
     (if (eq? d 3) (vector -1 0 0) (vector 0 0 0)))))

(define (bot-turn-left bot)
  (bot-modify-dir bot (- (bot-dir bot) 1)))

(define (bot-turn-right bot)
  (bot-modify-dir bot (+ (bot-dir bot) 1)))

(define (bot-forward bot)
  (bot-modify-pos bot (bot-in-front bot)))

(define (bot-backward bot)
  (bot-modify-pos bot (bot-behind bot)))

(define (bot-do-movement bot input)
  (bot-modify-dir
   (bot-modify-pos 
    bot
    (if (key-pressed "w")
        (bot-in-front bot)
        (if (key-pressed "s")
            (bot-behind bot)
            (bot-pos bot))))
   (+ (bot-dir bot)
      (if (key-pressed "a") 1 0)
      (if (key-pressed "d") -1 0))))

(define (bot-sequence bot l)
  (let ((step (modulo (bot-clock bot) (length l))))
    (apply (list-ref l step) (list bot))))
  
(define walker-bot
  '(lambda (bot input)
     (bot-sequence 
      bot
      (list
       bot-forward
       bot-forward
       bot-forward
       bot-forward
       bot-forward
       bot-turn-left))))

(define controlled-bot 
  '(lambda (bot input)
     (bot-do-movement
      (if (key-pressed "z") 
          (bot-modify-action bot 'dig)
          (if (key-pressed "x") 
              (bot-modify-action bot 'remove)
              bot))
      input)))

(define default-bot '(lambda (bot input) bot))

(define (bot-run-action bot octree)
  (cond 
   ((eq? (bot-action bot) 'dig)
    (octree-delete octree (vadd (vector 0 -1 0) (bot-pos bot))))
   ((eq? (bot-action bot) 'remove)
    (octree-delete octree (bot-in-front bot)))
   (else octree)))
  
(define (make-bots l) (list l))
(define (bots-list bs) (list-ref bs 0)) 
(define (bots-modify-list bs v) (list-replace bs 0 v)) 

(define (bots-add-bot bs bot)
  (bots-modify-list 
   bs 
   (cons bot (bots-list bs))))

(define (bots-run-code bs octree input bricks)
  (bots-modify-list
   bs
   (map
    (lambda (bot)
      (bot-run-code bot octree input bricks))
    (bots-list bs))))

(define (bots-run-actions bs octree)
  (foldl
    (lambda (bot octree)
      (bot-run-action bot octree))
    octree
    (bots-list bs)))

(define (bots-octree-change? bs)
  (foldl
   (lambda (bot r)
     (if (and (not r) (or 
                       (eq? (bot-action bot) 'dig)
                       (eq? (bot-action bot) 'remove)))
         #t r))
   #f
   (bots-list bs)))

(define (bots-clear-actions bs)
  (bots-modify-list
   bs
   (map
    (lambda (bot)
      (bot-modify-action bot 'none))
    (bots-list bs))))