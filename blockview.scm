;; al jazari two (c) 2013 dave griffiths gpl v3

(load "octree.scm")

(define built 0)

(define (pdata-list-set! k l v)
  (for-each (lambda (i) (pdata-set! k i v)) l))

(define (occ-samples pos) 
    (list 
     (vadd pos (vector  -1 -1 -1))
     (vadd pos (vector  -1 -1  0))
     (vadd pos (vector  -1  0 -1))
     (vadd pos (vector  -1  0  0))
     (vadd pos (vector   1 -1 -1))
     (vadd pos (vector   1 -1  0))
     (vadd pos (vector   0  0 -1))
     (vadd pos (vector   0  0  0))))


(define (calc-occlusion o pos)
  (let* ((samples (occ-samples pos))
         (cov (/ (foldl
                  (lambda (p r)
                    (+ (if (octree-leaf-empty? (octree-ref o p)) 1 0) r))
                  0
                  samples)
                 (length samples))))
    (min 1 (* 2 cov))))

(define (make-block o pos size value)
  (set! built (+ built 1))
  (with-state
   (hint-frustum-cull)
   ;(hint-vertcols)
    (wire-colour 1)
   (texture-params 0 '(min linear-mipmap-linear mag nearest))
;   (texture (load-texture (string-append "textures/" (number->string value) ".png")))
;   (texture (load-texture "textures/outline.png"))
   (colour (cond 
            ((eq? size 1) (vector 0 0 1))
            ((eq? size 2) (vector 0 0.25 1))
            ((eq? size 4) (vector 0 0.75 1))
            ((eq? size 8) (vector 0 1 1))
            ((eq? size 16) (vector 1 0.25 1))
            ((eq? size 32) (vector 1 0.75 1))))
   (translate pos)
   (scale size)
   (translate (vector 0.5 0.5 0.5))
   (let ((p (build-cube)))
     (with-primitive 
      p
      (pdata-map! (lambda (c) (vector 0.5 0.5 0.5)) "c")
      (pdata-map! (lambda (t) (vmul t size)) "t")
      #;(let ((b (+ size 0)) (a 0))
        (pdata-list-set! "c" '(10 14 21) (calc-occlusion o (vadd pos (vector b b b))))
        (pdata-list-set! "c" '(3 7 19) (calc-occlusion o (vadd pos (vector a a a))))
        (pdata-list-set! "c" '(2 4 23) (calc-occlusion o (vadd pos (vector b a a))))
        (pdata-list-set! "c" '(9 15 17) (calc-occlusion o (vadd pos (vector a b b))))
        (pdata-list-set! "c" '(0 8 18) (calc-occlusion o (vadd pos (vector a b a))))
        (pdata-list-set! "c" '(5 13 22) (calc-occlusion o (vadd pos (vector b a b))))
        (pdata-list-set! "c" '(6 12 16) (calc-occlusion o (vadd pos (vector a a b))))
        (pdata-list-set! "c" '(1 11 20) (calc-occlusion o (vadd pos (vector b b a)))))
      (recalc-bb))
     p)))

#;(define (block-view-builder oct)
  (octree-map
   (lambda (o pos size depth)
     (if (octree-leaf-viz o)
         (make-block oct pos size (octree-leaf-value o))
         'f))
   oct))
   
(define (block-view-builder oct o x y z depth)
  (let ((s (/ octree-size (expt 2 depth) 2)))
    (cond
     ((octree-leaf? o) 
      (if (octree-leaf-viz o) 
          (make-block oct (vector x y z) (* s 2) (octree-leaf-value o))
          'e))
     (else
      (vector
       (block-view-builder oct (octree-branch o 0) x y z (+ depth 1))
       (block-view-builder oct (octree-branch o 1) (+ x s) y z (+ depth 1))
       (block-view-builder oct (octree-branch o 2) x (+ y s) z (+ depth 1))
       (block-view-builder oct (octree-branch o 3) (+ x s) (+ y s) z (+ depth 1))
       (block-view-builder oct (octree-branch o 4) x y (+ z s) (+ depth 1))
       (block-view-builder oct (octree-branch o 5) (+ x s) y (+ z s) (+ depth 1))
       (block-view-builder oct (octree-branch o 6) x (+ y s) (+ z s) (+ depth 1))
       (block-view-builder oct (octree-branch o 7) (+ x s) (+ y s) (+ z s) (+ depth 1)))))))
  
(define (make-block-view octree)
  (block-view-builder octree octree 0 0 0 0))

(define (block-view-destroy bv)
  (define (_ o)
    (cond
     ((eq? o 'e) 'e)
     ((not (vector? o)) (destroy o) 'e)
     (else
      (vector
       (_ (octree-branch o 0))
       (_ (octree-branch o 1))
       (_ (octree-branch o 2))
       (_ (octree-branch o 3))
       (_ (octree-branch o 4))
       (_ (octree-branch o 5))
       (_ (octree-branch o 6))
       (_ (octree-branch o 7))))))
  (_ bv))
  
(define (block-view-update bv octree)
  (define (_ o b x y z depth)
    (let ((s (/ octree-size (expt 2 depth) 2)))
      (cond
       ;; check for different types
       ((or 
         (and (vector? o)
              (not (vector? b))) ;; shattered
         (and (vector? b)
            (not (vector? o)))) ;; compressed
        ;; rebuild this subtree
        (block-view-destroy b)
        (block-view-builder octree o x y z depth))
       ;; deleted block
       ((and (not (vector? o))
             (eq? o 'e)
             (not (eq? b 'e)))
        (block-view-destroy b)
        'e)
       ;; new block
       ((and (not (vector? o))
             (not (eq? o 'e))
             (eq? b 'e))
        (block-view-builder octree o x y z depth))
       ((not (vector? o)) ;; todo: are assuming type doesn't change 
        b)       
       ;; they are the same, continue
       (else
        (vector
         (_ (octree-branch o 0)
            (octree-branch b 0) x y z (+ depth 1))
         (_ (octree-branch o 1)
            (octree-branch b 1) (+ x s) y z (+ depth 1))
         (_ (octree-branch o 2)
            (octree-branch b 2) x (+ y s) z (+ depth 1))
         (_ (octree-branch o 3)
            (octree-branch b 3) (+ x s) (+ y s) z (+ depth 1))
         (_ (octree-branch o 4)
            (octree-branch b 4) x y (+ z s) (+ depth 1))
         (_ (octree-branch o 5)
            (octree-branch b 5) (+ x s) y (+ z s) (+ depth 1))
         (_ (octree-branch o 6)
            (octree-branch b 6) x (+ y s) (+ z s) (+ depth 1))
         (_ (octree-branch o 7)
            (octree-branch b 7) (+ x s) (+ y s) (+ z s) (+ depth 1)))))))
  (_ octree bv 0 0 0 0))


(define (blockview-setup-scene)
;  (set-camera-transform (mtranslate (vector 0 -3 -5)))

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
