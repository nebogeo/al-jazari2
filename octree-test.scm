;; al jazari two (c) 2013 dave griffiths gpl v3

(load "blockview.scm")

(define octree
  (octree-compress
  (octree-compress
  (octree-compress
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
      (vector 32 35 26) 8))))))))

(define block-view (make-block-view octree))
