;; al jazari two (c) 2013 dave griffiths gpl v3

(clear)

(load "botview.scm")
(load "blockview.scm")
(load "input.scm")

(define (setup-scene)
  (set-camera-transform (mtranslate (vector 0 -3 -5)))

  (light-diffuse 0 (vector 0.4 0.4 0.4))
  (light-specular 0 (vector 0.1 0.1 0.1))

  (let ((mylight (make-light 'spot 'free)))
    (light-position mylight (vector 200 500 -200))
    (light-diffuse mylight (vector 1 1 1))
    (light-spot-angle mylight 60)
    (light-spot-exponent mylight 10)
    (light-attenuation mylight 'constant 1) 
    (light-direction mylight (vector -0.2 -0.8 0.3))
    (shadow-light mylight)
    )
  
  (with-state
   (hint-unlit)
   (rotate (vector 180 0 0))
   (texture (load-texture "textures/bg.png"))    
   (scale 1000)
   (backfacecull 0)
   (texture-params 0 '(min nearest mag nearest))
   (build-sphere 20 20)))

#;(define octree
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-compress
      (octree-compress
       (octree-compress
        (octree-delete-sphere
         (octree-fill-sphere
          (octree-fill-sphere
           (octree-box 
            (make-empty-octree)        
            (vector 0 0 0)
            (vector 64 32 64) 0)
           (vector 32 32 32) 10 1)
          (vector 20 32 12) 5 2)
         (vector 32 35 26) 8))))))))

(define (octree-compresss octree)
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-compress
      (octree-compress
       (octree-compress octree)))))))
        
(define (octree-land octree)
  (octree-delete-box
   (octree-fill-sphere
    (octree-fill-sphere
     (octree-box 
      (make-empty-octree)
      (vector 0 0 0)
      (vector 64 64 64) 2)
     (vector 32 32 32) 20 4)
    (vector 32 15 32) 20 5)
   (vector 0 32 0) 
   (vector 64 64 64)))

(define (octree-tree pos size octree)
  (octree-fill-sphere
   (octree-box
    octree
    (vadd pos (vector 0 0 0))
    (vadd pos (vector 1 4 1)) 0)
   (vadd pos (vector 0 (+ 3 size) 0))
   size 1))

(define octree
  (octree-compresss
    (octree-land (make-empty-octree))))

#;(define octree
  (octree-compresss
   (octree-box
    (make-empty-octree)
    (vector 0 0 0)
    (vector 64 30 64) 2)))
  
(define (add-bot bots id pos bricks)
  (bots-add-bot 
   bots
   (make-bot
    id
    pos 
    ;; assume code was last added, at end of list
    (brick-id (car (reverse (bricks-roots bricks)))))))

(define block-view (make-block-view octree))
(define bots (make-bots '()))
(define bot-views (make-bot-views '()))
(define input (make-input))
(define canvas (build-locator))
(define bricks (make-bricks canvas))
(setup-scene)
(define pointer (with-state 
                 (parent canvas) 
                 (scale 0.1)
                 (colour (vector 1 1 0))
                 (build-cube)))

(set! bricks (bricks-add-code bricks controlled-bot))
(set! bots (add-bot bots 0 (vector 27 36 26) bricks))
(set! bot-views (bot-views-update bot-views bots bricks))

#;((set! bricks (bricks-add-code bricks default-bot))
(set! bots (add-bot bots 1 (vector 25 36 26) bricks))
(set! bot-views (bot-views-update bot-views bots bricks))

(set! bricks (bricks-add-code bricks default-bot))
(set! bots (add-bot bots 2 (vector 23 36 26) bricks))
(set! bot-views (bot-views-update bot-views bots bricks))
)

(define (wall n)
  (loop
   n x
   pickup
   forward
   drop
   turn-left
   forward
   turn-left
   forward
   turn-right
   turn-right))

(define (ridge-trench size gap)
  (seq
   (repeat size pos
    pickup
    (repeat (+ (* pos 2) 1 gap) n 
     forward)  ;; move forward leaving steps
    drop
    turn-left
    turn-left
    (repeat (+ (* (+ pos 1) 2) gap) pos 
     forward) ;; move back to the last pos
    turn-left
    turn-left)
   (repeat size pos  ;; move back to 
    forward))) ;; start position

(define (plateau size gap)
  (seq
   (repeat size row
    (ridge-trench size gap)
    turn-left   ;; move sideways 
    forward     ;; for next trench
    turn-right) 
   turn-right ;; move back
   (repeat size row  ;; to start position
         forward)
   turn-left))

(define (pyramid size)
  (repeat
   (/ size 2) level
   (plateau (- size (* level 2)) (+ (* level 2) 1))
   backward
   turn-left
   forward
   turn-right))
  

(define size 8)

(set! bricks (bricks-add-code bricks   
                              '(lambda (bot octree)
                                 (bot-sequence 
                                  bot

(repeat
   (/ size 2) level
   (plateau (- size (* level 2)) (+ (* level 2) 1))
   backward
   turn-left
   forward
   turn-right)

                                        ;  (pyramid 8)
                                  ))))

(set! bots (add-bot bots 3 (vector 19 36 26) bricks))
(set! bot-views (bot-views-update bot-views bots bricks))

(define cam-id 0)
(define camera (bot-view-prim (list-ref-safe bot-views cam-id)))
(lock-camera camera)
(with-primitive canvas (parent camera))

(define (update)
  (let ((tx (with-primitive camera (get-transform)))
        (pos (vtransform 
              (vector 0 0 0) 
              (with-primitive 
               pointer
               (hide 1)
               (identity)
               (translate (get-point-from-mouse))
               (scale 0.1)
               (get-global-transform))))
        (loc-pos (vtransform (vector 0 0 0) (with-primitive pointer (get-transform)))))

    (when (input-key? input " ")
          (set! cam-id (+ 1 cam-id))
          (let ((camera (bot-view-prim (list-ref-safe bot-views cam-id))))
            (lock-camera camera)
            (with-primitive canvas (parent camera))))

    (set! input (input-update input))
    (set! bots (bots-run-code bots octree input bricks))
    (set! octree (bots-run-actions bots octree))

    (when (bots-octree-change? bots)
          (set! octree 
                (octree-compress octree))
          (set! block-view 
                (block-view-update 
                 block-view octree)))

    (set! bots (bots-clear-actions bots))
    (set! bot-views (bot-views-update bot-views bots bricks))
    (set! bricks (bricks-update! bricks tx pos loc-pos))
))

(show-fps 1)
(every-frame (update))

(unlock-camera)

(display (length (loop-explode (pyramid 8))))(newline)

;(start-framedump "frames/comp-" "jpg")