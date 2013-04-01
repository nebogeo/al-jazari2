;; al jazari two (c) 2013 dave griffiths gpl v3

(clear)
(load "blockview.scm")

#;(define octree
    (octree-calc-viz
        (octree-multi-compress
         (octree-fill-sphere
           (make-empty-octree)
           (vector 39 32 32) 35 5)

            )))

(blockview-setup-scene)

(random-seed 23)

(define octree
  (octree-calc-viz
   (octree-multi-compress
    (octree-forest 10 0
                   (blob-land 22 2 
                              (make-empty-octree)    
                              )))))
;(hint-wire)
;(hint-unlit)

(define block-view 
    (with-state
        (translate (vector -32 -32 -32))
        (make-block-view octree)))

(display built)(newline)