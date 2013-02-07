;; al jazari two (c) 2013 dave griffiths gpl v3

(load "list.scm")
(load "octree.scm")
(load "input.scm")
(load "scheme-bricks.scm")

(define bot-cycle-time 1.3)

;; ----------------------------------------------------
;; thinking machines

(define (make-bot id pos brick-id) (list id pos 0 0 brick-id 'none (+ (time) (random)) #f))
(define (bot-id b) (list-ref b 0))
(define (bot-pos b) (list-ref b 1))
(define (bot-dir b) (list-ref b 2))
(define (bot-clock b) (list-ref b 3))
(define (bot-brick-id b) (list-ref b 4))
(define (bot-action b) (list-ref b 5))
(define (bot-time b) (list-ref b 6))
(define (bot-carrying b) (list-ref b 7))
(define (bot-modify-pos b v) (list-replace b 1 v))
(define (bot-modify-dir b v) (list-replace b 2 v))
(define (bot-modify-clock b v) (list-replace b 3 v))
(define (bot-modify-action b v) (list-replace b 5 v))
(define (bot-modify-time b v) (list-replace b 6 v))
(define (bot-modify-carrying b v) (list-replace b 7 v))

;; actions:
;; pickup - pick up block underneath
;; drop - put block down underneath

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
                                  (list bot octree)))
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

;; actions for sequencing (all same form)

(define (bot-turn-left bot octree)
  (bot-modify-dir bot (+ (bot-dir bot) 1)))

(define (bot-turn-right bot octree)
  (bot-modify-dir bot (- (bot-dir bot) 1)))

(define (bot-forward bot octree)
  ;; check edge of world and space clear
  (let ((pos (bot-in-front bot)))
    (if (and
         (>= (vx pos) 0) (< (vx pos) octree-size)
         (>= (vz pos) 0) (< (vz pos) octree-size)
         (or (eq? (octree-ref octree pos) 'e)
             (eq? (octree-ref octree (vadd (vector 0 1 0)
                                           (bot-in-front bot))) 'e)))
        (bot-modify-pos bot pos)
        bot)))

(define (bot-backward bot octree)
  (bot-modify-pos bot (bot-behind bot)))

(define (bot-underneath bot)
  (vadd (bot-pos bot) (vector 0 -1 0)))

(define (bot-ontop bot)
  (vadd (bot-pos bot) (vector 0 1 0)))

(define (bot-pickup bot octree)
  (if (not (bot-carrying bot))
      (bot-modify-carrying 
       (bot-modify-action bot 'pickup)
       (octree-ref octree (bot-underneath bot))) 
      bot))

(define (bot-drop bot octree)
  (if (bot-carrying bot)
      ;; the the space in front is free
      (if #f #;(eq? (octree-ref octree (bot-in-front bot)) 'e)
          (bot-modify-action bot 'drop-in-front)
          ;; otherwise go underneath
          (bot-modify-action       
           (bot-modify-pos bot (bot-ontop bot))
           'drop))
      bot))

;; hmm, should be in the view

(define (bot-hide-code bot)
  (with-primitive (bot-brick-id bot) (hide 1))
  bot)

(define (bot-unhide-code bot)
  (with-primitive (bot-brick-id bot) (hide 0))
  bot)


;;--

(define (bot-run-action bot octree)
  (cond 
   ((eq? (bot-action bot) 'pickup)
    (octree-delete octree (bot-underneath bot)))
   ((eq? (bot-action bot) 'drop)
    (octree-set octree (vadd (bot-pos bot) (vector 0 -1 0)) (bot-carrying bot)))
   ((eq? (bot-action bot) 'drop-in-front)
    (octree-set octree (bot-in-front bot) (bot-carrying bot)))
   (else octree)))
  
;;-------------------------------------------------------------
;; a collection of bots

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
     (if (and 
          (not r) 
          (or 
           (eq? (bot-action bot) 'pickup)
           (eq? (bot-action bot) 'drop)
           (eq? (bot-action bot) 'drop-in-front)))
         #t r))
   #f
   (bots-list bs)))

(define (bots-clear-actions bs)
  (bots-modify-list
   bs
   (map
    (lambda (bot)
      (bot-modify-action 
       (if (or (eq? (bot-action bot) 'drop-in-front)
               (eq? (bot-action bot) 'drop)
               (eq? (bot-action bot) 'place))
           (bot-modify-carrying bot #f)
           bot)
       'none))
    (bots-list bs))))

;; experimental/utility

(define (bot-do-movement bot octree)
  (if (key-pressed "h")
      (bot-hide-code bot)
      (bot-modify-dir
       (if (key-pressed "w")
            (bot-forward bot octree)
            (if (key-pressed "s")
                (bot-backward bot octree)
                bot))
       (+ (bot-dir bot)
          (if (key-pressed "a") 1 0)
          (if (key-pressed "d") -1 0)))))

(define (bot-sequence bot l)
  (let ((step (modulo (bot-clock bot) (length l))))
    (apply (list-ref l step) (list bot octree))))
  
(define controlled-bot 
  '(lambda (bot octree)
     (bot-do-movement
      (if (key-pressed "x") 
          (bot-pickup bot octree)
          (if (key-pressed "c") 
              (bot-drop bot octree)
              bot))
      octree)))

(define default-bot '(lambda (bot octree) bot))
