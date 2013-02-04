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




(define bricks
  (with-state
;  (parent camera)
  ;; (translate (vector 0 5 0))
   (bricks-add-code
    (make-bricks) 
    '(hello (there)(1 2 3)))))

(define bots (make-bots 
              (list 
               (make-bot 
                0 (vector 20 26 20) 
                (lambda (bot)                              
                  (bot-modify-dir
                   (bot-modify-pos 
                    (if (key-pressed "z") 
                        (bot-modify-action bot 'dig)
                        bot)
                    (if (key-pressed "w")
                        (vadd (bot-pos bot) 
                              (if (eq? (bot-dir bot) 0) (vector 0 0 -1) (vector 0 0 0))
                              (if (eq? (bot-dir bot) 1) (vector -1 0 0) (vector 0 0 0))
                              (if (eq? (bot-dir bot) 2) (vector 0 0 1) (vector 0 0 0))
                              (if (eq? (bot-dir bot) 3) (vector 1 0 0) (vector 0 0 0)))
                        (if (key-pressed "s")
                            (vadd (bot-pos bot) 
                                  (if (eq? (bot-dir bot) 0) (vector 0 0 1) (vector 0 0 0))
                                  (if (eq? (bot-dir bot) 1) (vector 1 0 0) (vector 0 0 0))
                                  (if (eq? (bot-dir bot) 2) (vector 0 0 -1) (vector 0 0 0))
                                  (if (eq? (bot-dir bot) 3) (vector -1 0 0) (vector 0 0 0)))
                            (bot-pos bot))))
                   (modulo (+ (bot-dir bot)
                              (if (key-pressed "a") -1 0)
                              (if (key-pressed "d") 1 0)) 4))))
              (make-bot 1 (vector 22 26 20) (lambda (bot) bot)))))


(define bot-views (make-bot-views '()))

(set! bot-views (bot-views-update bot-views bots))

(define camera (bot-view-prim (car bot-views)))
(lock-camera camera)

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
; (colour (vector 0.3 0.8 1))
(rotate (vector 180 0 0))
 (texture (load-texture "textures/bg.png"))    
 (scale 1000)
 (backfacecull 0)
(texture-params 0 '(min nearest mag nearest))
 (build-sphere 20 20))

(camera-lag 0.9)

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
