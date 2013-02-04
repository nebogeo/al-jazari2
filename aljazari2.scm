(clear)

(load "botview.scm")
(load "blockview.scm")
(load "scheme-bricks.scm")
(load "input.scm")

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

(define block-view (make-block-view octree))
;(destroy-block-view-path block-view (list 0 7))

(define bots (make-bots 
              (list 
               (make-bot 
                0 (vector 20 46 20) 
                (lambda (bot input)                              
                  (bot-modify-dir
                   (bot-modify-pos 
                    (if (input-key? input "z") 
                        (bot-modify-action bot 'dig)
                        (if (input-key? input "x") 
                            (bot-modify-action bot 'remove)
                            bot))
                    (if (input-key? input "w")
                        (bot-in-front bot)
                        (if (input-key? input "s")
                            (bot-behind bot)
                            (bot-pos bot))))
                   (+ (bot-dir bot)
                      (if (input-key? input "a") 1 0)
                      (if (input-key? input "d") -1 0)))))
              (make-bot 1 (vector 22 46 20) (lambda (bot input) bot)))))

(define bot-views (make-bot-views '()))
(set! bot-views (bot-views-update bot-views bots))

(define camera (bot-view-prim (car bot-views)))
(lock-camera camera)

(define canvas (with-state
                (parent camera) 
                (build-locator)))

(define input (make-input))

(define bricks
  (with-state
   (parent canvas)
   ;; (translate (vector 0 5 0))
   (bricks-add-code
    (make-bricks canvas) 
    '(hello (there)(1 2 3)))))

(with-primitive
 (brick-id (car (bricks-roots bricks)))
 (scale 0.1)
 (translate (vector -3 15 0)))
 

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
 (build-sphere 20 20))

(define pointer (with-state 
                 (parent canvas) 
                 (scale 0.1) 
                 (colour (vector 1 1 0))
                 (build-cube)))

(define (update)
  (let ((tx (with-primitive camera (get-transform))))
    (set! input (input-update input))
    (set! bots (bots-run-code bots octree input))
    (set! octree (bots-run-actions bots octree))

   (with-primitive pointer
                   (identity)
                   (translate (get-point-from-mouse))
                   (scale 0.1))


    (when (bots-octree-change? bots)
          (set! octree 
                (octree-compress octree))
          (set! block-view 
                (block-view-update 
                 block-view octree)))

    (set! bots (bots-clear-actions bots))
    (set! bot-views (bot-views-update bot-views bots))
    (set! bricks (bricks-update! bricks tx))))

(show-fps 1)
(every-frame (update))
