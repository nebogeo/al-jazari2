(clear)

(load "botview.scm")
(load "blockview.scm")

(define octree
  (octree-fill-sphere
   (octree-delete-sphere
    (octree-fill-sphere
     (octree-box 
      (make-empty-octree)        
      (vector 0 0 0)
      (vector 64 32 64) 0)
     (vector 32 32 32) 10 1)
    (vector 32 35 26) 8)
   (vector 20 32 12) 5 2))

(define block-view (make-block-view octree))
;(destroy-block-view-path block-view (list 0 7))

(define bots (make-bots octree
                        (list 
                         (make-bot 0 (vector 20 26 20) '()))))

(define bot-views (make-bot-views '()))


(set! bot-views (bot-views-update bot-views bots))
(lock-camera (bot-view-prim (car bot-views)))

(every-frame (begin

               (when (key-pressed "o")
                     (set! octree 
                           (octree-compress octree))
                     (set! block-view 
                           (block-view-update 
                            block-view octree)))


               (when (key-pressed "p")
                     (set! octree 
                           (octree-delete-sphere 
                            octree (vector 15 32 12) 6))
                     (set! block-view 
                           (block-view-update 
                            block-view octree)))

               (set! bots (bots-run bots))
               (set! bot-views (bot-views-update bot-views bots))))
