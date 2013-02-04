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
                                        ;(shadow-light mylight)
    )
  
  (with-state
   (hint-unlit)
   (rotate (vector 180 0 0))
   (texture (load-texture "textures/bg.png"))    
   (scale 1000)
   (backfacecull 0)
   (texture-params 0 '(min nearest mag nearest))
   (build-sphere 20 20)))

(define octree
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-delete-sphere
;      (octree-fill-sphere
       (octree-fill-sphere
        (octree-fill-sphere
         (octree-box 
          (make-empty-octree)        
          (vector 0 0 0)
          (vector 64 32 64) 0)
         (vector 32 32 32) 10 1)
        (vector 20 32 12) 5 2)
 ;      (vector 40 28 20) 10 2)
      (vector 32 35 26) 8)))))

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
(set! bots (add-bot bots 0 (vector 20 46 20) bricks))
(set! bricks (bricks-add-code bricks default-bot))
(set! bots (add-bot bots 1 (vector 22 46 20) bricks))
(set! bot-views (bot-views-update bot-views bots))

(define camera (bot-view-prim (car bot-views)))
(lock-camera camera)
(with-primitive canvas (parent camera))

(define (update)
  (let ((tx (with-primitive camera (get-transform)))
        (pos (vtransform 
              (vector 0 0 0) 
              (with-primitive 
               pointer
               (identity)
               (translate (get-point-from-mouse))
               (scale 0.1)
               (get-global-transform)))))

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
    (set! bot-views (bot-views-update bot-views bots))
    (set! bricks (bricks-update! bricks tx pos))))

(show-fps 1)
(every-frame (update))
