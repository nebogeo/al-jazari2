(clear)

(load "botview.scm")
(load "blockview.scm")
(load "scheme-bricks.scm")

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
               (make-bot 
                0 (vector 20 26 20) 
                (lambda (bot)
                  (let ((dir (vadd
                              (if (key-pressed "w") (vector 0 0 -1) (vector 0 0 0))
                              (if (key-pressed "s") (vector 0 0 1) (vector 0 0 0))
                              (if (key-pressed "a") (vector -1 0 0) (vector 0 0 0))
                              (if (key-pressed "d") (vector 1 0 0) (vector 0 0 0)))))
                    (bot-modify-pos 
                     (if (key-pressed "z") 
                         (bot-modify-action bot 'dig)
                         bot)
                     (vadd (bot-pos bot) dir)))))
               (make-bot 1 (vector 22 26 20) (lambda (bot) bot)))))

(define bot-views (make-bot-views '()))

(set! bot-views (bot-views-update bot-views bots))

(define camera (bot-view-prim (car bot-views)))
(lock-camera camera)

(define bricks
  (with-state
  (parent camera)
  ;; (translate (vector 0 5 0))
   (bricks-add-code
    (make-bricks) 
    '(hello (there)(1 2 3)))))

(set-camera-transform (mtranslate (vector 0 -3 -10)))

(define (update)
  (let ((tx (with-primitive camera (get-transform))))

    (set! bots (bots-run-code bots octree))
    (set! octree (bots-run-actions bots octree))

    (when (bots-octree-change? bots)
          (set! octree 
                (octree-compress octree))
          (set! block-view 
                (block-view-update 
                 block-view octree)))

    (set! bots (bots-clear-actions bots))
    (set! bot-views (bot-views-update bot-views bots))
    (set! bricks (bricks-update! bricks tx))))

(every-frame (update))
