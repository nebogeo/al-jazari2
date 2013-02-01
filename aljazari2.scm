(clear)

(load "botview.scm")
(load "blockview.scm")

(define octree
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-fill-sphere
      (octree-delete-sphere
       (octree-fill-sphere
        (octree-box 
         (make-empty-octree)        
         (vector 0 0 0)
         (vector 64 32 64) 0)
        (vector 32 32 32) 10 1)
       (vector 32 35 26) 8)
      (vector 20 32 12) 5 2)))))

(define block-view (make-block-view octree))
;(destroy-block-view-path block-view (list 0 7))

(define bots (make-bots 
              (list 
               (make-bot 0 (vector 20 26 20) '()))))

(define bot-views (make-bot-views '()))

(set! bot-views (bot-views-update bot-views bots))
(lock-camera (bot-view-prim (car bot-views)))

(define (update)
  (set! bots (bots-run-code bots octree))
  (set! octree (bots-run-actions bots octree))
  
  (when (bots-octree-change? bots)
        (set! octree 
              (octree-compress octree))
        (set! block-view 
              (block-view-update 
               block-view octree)))
  
  (set! bots (bots-clear-actions bots))
  (set! bot-views (bot-views-update bot-views bots)))

(every-frame (update))
