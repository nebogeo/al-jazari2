;; al jazari two (c) 2013 dave griffiths gpl v3

(define octree-size 64)
(define octree-depth 6)

(define (octree-leaf viz v) (list viz v))
(define (octree-leaf-viz l) (list-ref l 0))
(define (octree-leaf-value l) (list-ref l 1))
(define (octree-leaf-empty? l) (eq? (octree-leaf-value l) 'e))
(define (octree-empty) (octree-leaf #f 'e))
(define (make-octree v) (vector v v v v v v v v))
(define (make-empty-octree) (make-octree (octree-empty)))
(define (octree-replace o i v) (vector-set! o i v) o)
(define octree-branch vector-ref)
(define (octree-leaf? o) (not (vector? o)))
(define (octree-branch-leaf? o i) (octree-leaf? (vector-ref o i)))
(define (octree-empty? o) (equal? o (vector (octree-empty) (octree-empty) (octree-empty) (octree-empty)
                                            (octree-empty) (octree-empty) (octree-empty) (octree-empty))))

(define (octree-contig? o) 
  (let ((v (octree-branch o 0)))
    (and
     (equal? (octree-branch o 1) v)
     (equal? (octree-branch o 2) v)
     (equal? (octree-branch o 3) v)
     (equal? (octree-branch o 4) v)
     (equal? (octree-branch o 5) v)
     (equal? (octree-branch o 6) v)
     (equal? (octree-branch o 7) v))))
  
(define (octree-path pos)
  (define (_ x y z depth)
    (cond 
      ((eq? depth octree-depth) '())
      (else
       (let ((split (/ octree-size (expt 2 (+ depth 1)))))
         (cons
          (bitwise-ior
           (if (< x split) 0 1)
           (if (< y split) 0 2)
           (if (< z split) 0 4))
          (_ (if (>= x split) (- x split) x)
             (if (>= y split) (- y split) y)
             (if (>= z split) (- z split) z) (+ depth 1)))))))
  (_ (vector-ref pos 0)
     (vector-ref pos 1)
     (vector-ref pos 2) 0))

(define (octree-ref o pos)
  (define (_ o path)
    (cond
      ((octree-branch-leaf? o (car path)) 
       (octree-branch o (car path)))
      (else
       (_ (octree-branch o (car path)) (cdr path)))))
  (_ o (octree-path pos)))

(define (octree-set o pos v)
  (define (_ o path v depth)
    (cond
      ;; at bottom of the tree?
      ((or
        (null? path)
        (eq? depth octree-depth))
       v)
      ;; reached a collapsed tree?
      ((octree-branch-leaf? o (car path))
       (let ((value (octree-branch o (car path))))
         (octree-replace
          o (car path)
          (_ (make-octree value)
             (cdr path) v (+ depth 1)))))
      ;; drill down existing path
      (else
       (octree-replace
        o (car path)
        (_ (octree-branch o (car path))
           (cdr path) v (+ depth 1))))))
  (_ o (octree-path pos) v 0))
       
(define (octree-delete o pos)
  (octree-set o pos (octree-leaf #f 'e)))

;; collapse chunks into single values, empty space or solid objects
(define (octree-compress o)
  (define (_ o)
    (cond
      ((octree-leaf? o) o) ;; it's a leaf
      (else
       (let ((r (vector
                 (_ (octree-branch o 0))
                 (_ (octree-branch o 1))
                 (_ (octree-branch o 2))
                 (_ (octree-branch o 3))
                 (_ (octree-branch o 4))
                 (_ (octree-branch o 5))
                 (_ (octree-branch o 6))
                 (_ (octree-branch o 7)))))
         ;; if all branches are leaves which the same, collapse them to one value
         (if (octree-contig? r) (octree-branch o 0) r)))))
  (_ o))

(define (print-tab s)
  (for-each (lambda (_) (display "-")) (build-list s (lambda (_) 0))))

(define (octree-foldl f v o)
  (define (_ o x y z depth)
    (let ((s (/ octree-size (expt 2 depth) 2)))
      (cond
       ((octree-leaf? o) 
        (if (not (octree-leaf-empty? o))
            (f o (vector x y z) (* s 2) depth v)
            v))
       (else
        (vector
         (_ (octree-branch o 0) x y z (+ depth 1))
         (_ (octree-branch o 1) (+ x s) y z (+ depth 1))
         (_ (octree-branch o 2) x (+ y s) z (+ depth 1))
         (_ (octree-branch o 3) (+ x s) (+ y s) z (+ depth 1))
         (_ (octree-branch o 4) x y (+ z s) (+ depth 1))
         (_ (octree-branch o 5) (+ x s) y (+ z s) (+ depth 1))
         (_ (octree-branch o 6) x (+ y s) (+ z s) (+ depth 1))
         (_ (octree-branch o 7) (+ x s) (+ y s) (+ z s) (+ depth 1)))))))
  (_ o 0 0 0 0))

(define (octree-map f o)
  (define (_ o x y z depth)
    (let ((s (/ octree-size (expt 2 depth) 2)))
      (cond
       ((octree-leaf? o) 
        (if (not (octree-leaf-empty? o))
            (f o (vector x y z) (* s 2) depth)
            o))
       (else
        (vector
         (_ (octree-branch o 0) x y z (+ depth 1))
         (_ (octree-branch o 1) (+ x s) y z (+ depth 1))
         (_ (octree-branch o 2) x (+ y s) z (+ depth 1))
         (_ (octree-branch o 3) (+ x s) (+ y s) z (+ depth 1))
         (_ (octree-branch o 4) x y (+ z s) (+ depth 1))
         (_ (octree-branch o 5) (+ x s) y (+ z s) (+ depth 1))
         (_ (octree-branch o 6) x (+ y s) (+ z s) (+ depth 1))
         (_ (octree-branch o 7) (+ x s) (+ y s) (+ z s) (+ depth 1)))))))
  (_ o 0 0 0 0))

(define octree-for-each octree-map)

(define (octree-check-edge f o pos size)
  (define (do-x x y)
    (cond
     ((eq? x -1) #f)
     ((octree-leaf-empty? (octree-ref o (vadd pos (f x y)))) #t)
     (else (do-x (- x 1) y))))
  (define (do-y y)
    (cond
     ((eq? y -1) #f)
     ((do-x size y) #t)
     (else (do-y (- y 1)))))
  (do-y size))

(define (octree-is-visible? o pos size)
  (or
   (octree-check-edge (lambda (x y) (vector size x y)) o pos size)
   (octree-check-edge (lambda (x y) (vector -1 x y)) o pos size)
   (octree-check-edge (lambda (x y) (vector x size y)) o pos size)
   (octree-check-edge (lambda (x y) (vector x -1 y)) o pos size)
   (octree-check-edge (lambda (x y) (vector x y size)) o pos size)
   (octree-check-edge (lambda (x y) (vector x y -1)) o pos size)))

(define (octree-calc-viz o)
  (octree-map
   (lambda (v pos size depth)
     (octree-leaf
      (octree-is-visible?
       o pos size)
      (octree-leaf-value v)))
     o))


(define (octree-print o)
  (octree-for-each 
   (lambda (o pos size depth)
     (print-tab depth)(display o)(newline))))
       
(define (index->coords i)
  (vector
   (modulo i octree-size)
   (modulo (quotient i octree-size) octree-size)
   (quotient i (* octree-size
                  octree-size))))

(define (vdist a b)
  (let ((vx (- (vector-ref b 0) (vector-ref a 0)))
        (vy (- (vector-ref b 1) (vector-ref a 1)))
        (vz (- (vector-ref b 2) (vector-ref a 2))))    
    (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))

(define (octree-fold o fn)
  (foldl
   (lambda (i o)
     (fn o (index->coords i)))
   o
   (build-list (* octree-size
                  octree-size 
                  octree-size)
               (lambda (i) i))))

(define (octree-cut o y)
  (octree-fold 
   o 
   (lambda (o pos)
     (if (> (vector-ref pos 2) y)
         (octree-delete o pos)
         o))))

(define (inside? pos min max)
  (and
   (>= (vector-ref pos 0) (vector-ref min 0))
   (< (vector-ref pos 0) (vector-ref max 0))
   (>= (vector-ref pos 1) (vector-ref min 1))
   (< (vector-ref pos 1) (vector-ref max 1))
   (>= (vector-ref pos 2) (vector-ref min 2))
   (< (vector-ref pos 2) (vector-ref max 2))))

(define (octree-box o min max v)
  (octree-fold
   o
   (lambda (o pos)
     (if (inside? pos min max)
         (octree-set o pos (octree-leaf #t v))
         o))))

(define (octree-delete-box o min max)
  (octree-fold
   o
   (lambda (o pos)
     (if (inside? pos min max)
         (octree-delete o pos)
         o))))

(define (octree-fill-sphere o pos size v)
  (octree-fold
   o
   (lambda (o ipos)
     (if (< (vdist ipos pos) size)
         (octree-set o ipos (octree-leaf #t v))
         o))))

(define (octree-delete-sphere o pos size)
  (octree-fold
   o
   (lambda (o ipos)
     (if (< (vdist ipos pos) size)
         (octree-delete o ipos)
         o))))

(define (octree-multi-compress octree)
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-compress
      (octree-compress
       (octree-compress octree)))))))
        
(define (octree-land octree)
  (octree-fill-sphere
   (octree-delete-box
    (octree-fill-sphere
     (octree-box 
      (make-empty-octree)
      (vector 0 0 0)
      (vector 64 64 64) 2)
     (vector 32 32 32) 20 4)
    (vector 0 32 0) 
    (vector 64 64 64))
   (vector 32 15 32) 20 5))

(define (rr a b) (+ a (random (- b a))))

(define (octree-tree pos size base octree)
  (let ((width (/ size 3)))
    (octree-fill-sphere
     (octree-box
      octree
      (vadd pos (vector (+ (- width) 1) (- base (vy pos)) (+ (- width) 1)))
      (vadd pos (vector width 4 width)) 0)
     (vadd pos (vector 0 (+ 3 size) 0))
     size 1)))

(define (octree-forest c b o)
  (foldl
   (lambda (i r)
     (octree-tree 
      (vector (rr 0 64) (rr 25 38) (rr 0 64)) 
      (rr 3 8) b o))
   o
   (build-list c (lambda (c) c))))

(define (blob-land size v o)
  (define (_x x y)
    (cond 
     ((zero? x) o)
     (else
      (octree-fill-sphere
       (_x (- x 1) y)
       (vector (+ (* x 10) (rr -15 15))
               1
               (+ (* y 10) (rr -15 15)))
       (+ size (random 6))
       v))))
  (define (_y o x y)
    (cond 
     ((zero? y) o)
     (else
      (_y (_x x y) x (- y 1)))))
  (_y o 8 8))


(define (test)
  (define o (make-empty-octree))
  
  (octree-ref o 30 30 13)
  ;(set! o (octree-set o 30 30 13 '111))
  ;(set! o (octree-set o 0 30 133 '222))
  (set! o (octree-set o 230 235 13 '333))
  (set! o (octree-set o 30 30 14 '444))
  ;(set! o (octree-set o 30 31 14 '555))
  
  
  (octree-ref o 30 30 14)
  (set! o (octree-delete o 30 30 14))
  (set! o (octree-delete o 230 235 13))
  (octree-ref o 30 30 14)
  
  (octree-print o)
  (display o)(newline)
  (set! o (octree-compress o))
  (display o)(newline))
  
