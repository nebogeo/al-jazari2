;; al jazari two (c) 2013 dave griffiths gpl v3

(load "bots.scm")

(define (make-bot-view id prim carry-prim) (cons id (list prim 99 (vector 0 0 0) 0 carry-prim #f)))
(define (bot-view-id bv) (car bv))
(define (bot-view-prim bv) (list-ref (cdr bv) 0))
(define (bot-view-t bv) (list-ref (cdr bv) 1))
(define (bot-view-old-pos bv) (list-ref (cdr bv) 2))
(define (bot-view-old-dir bv) (list-ref (cdr bv) 3))
(define (bot-view-carry-prim bv) (list-ref (cdr bv) 4))
(define (bot-view-old-carry bv) (list-ref (cdr bv) 5))
(define (bot-view-modify-t bv v) (cons (car bv) (list-replace (cdr bv) 1 v)))
(define (bot-view-modify-old-pos bv v) (cons (car bv) (list-replace (cdr bv) 2 v)))
(define (bot-view-modify-old-dir bv v) (cons (car bv) (list-replace (cdr bv) 3 v)))
(define (bot-view-modify-old-carry bv v) (cons (car bv) (list-replace (cdr bv) 5 v)))

(define (veq? a b)
  (and (eq? (vx a) (vx b))
       (eq? (vy a) (vy b))
       (eq? (vz a) (vz b))))

(define (lerp-angle a b t)
  (fmod (lerp a b t) 4))

(define (bot-view-update-carry bv bot)
  (if (not (eq? (bot-carrying bot) 
                (bot-view-old-carry bv)))
      (with-primitive 
       (bot-view-carry-prim bv)
       (if (not (bot-carrying bot))
           (hide 1)
           (begin
             (hide 0)
             (texture 
              (load-texture 
               (string-append "textures/" 
                              (number->string (bot-carrying bot))
                              ".png")))))
       (bot-view-modify-old-carry bv (bot-carrying bot)))
      bv))

(define (bot-view-update-animation bv bot)
  (with-primitive 
   (bot-view-prim bv)
   ;; blend...
   (identity)
   (translate (vector 0.5 0.5 0.5))
   (translate (vlerp (bot-view-old-pos bv) (bot-pos bot) (bot-view-t bv)))
   (rotate (vector 0 (* (lerp-angle (bot-view-old-dir bv) 
                                    (bot-dir bot)
                                    (bot-view-t bv)) 90) 0))
   (bot-view-modify-t bv (+ (bot-view-t bv) 0.2))))

(define (bot-view-update-move-end bv bot)
  ;; end of move!
  (with-primitive 
   (bot-view-prim bv)
   ;; snap
   (identity)
   (translate (vector 0.5 0.5 0.5))
   (translate (vlerp (bot-view-old-pos bv) (bot-pos bot) 1))
   (rotate (vector 0 (* (lerp-angle (bot-view-old-dir bv) 
                                    (bot-dir bot)
                                    1) 90) 0))
   (bot-view-modify-old-pos
    (bot-view-modify-old-dir
     (bot-view-modify-t 
      (bot-view-update-carry bv bot)
      99)
     (bot-dir bot))
    (bot-pos bot))))
 
(define (bot-view-update bv bot bricks)
  ;; update the scheme brick position
  (with-primitive 
   (bot-brick-id bot)
   (identity)
   (scale 0.15)
   (translate 
    (vector 
     -3 
     (+ 5 (brick-children-size (bricks-search bricks (bot-brick-id bot))) )
     0)))
  ;; if moving
  (if (not (eq? (bot-view-t bv) 99))
      (if (< (bot-view-t bv) 1)
          (bot-view-update-animation bv bot)
          (bot-view-update-move-end bv bot))
      ;; if 99 then detect if a new move happened
      (if (or (not (veq? (bot-view-old-pos bv) (bot-pos bot)))
              (not (eq? (bot-view-old-dir bv) (bot-dir bot))))
          (bot-view-modify-t bv 0)
          bv)))

(define (expand distance)
  (pdata-map!
   (lambda (p n)
     (vadd p (vmul n distance)))
   "p" "n"))

(define (cheap-toon obj pen-width pen-colour)
  (with-state
    ; copy and parent a new object
    (parent obj)
    (let ((outline (with-primitive 
                    obj 
                    (poly-build-triangulate obj))))
      (with-primitive 
       outline
       (recalc-normals 1)
       ;; setup toon appearance
       ;;(poly-convert-to-indexed)
       (hint-unlit)
       (colour pen-colour)
       ;; grow and flip object inside out
       (expand pen-width)
       ;;(hint-cull-ccw)
       (recalc-normals 0))))
    ;;(with-primitive obj (recalc-normals 0))
  )


(define (bot-view-create bot)
  (let* ((p (with-state
             (scale 0.5)
             (translate (vector 0 -1.2 0))
             ;;(hint-unlit)
             (hint-cast-shadow)
             (colour (list-ref-safe 
                      (list
                       (vector 1 0 0)
                       (vector 0 1 0)
                       (vector 0 0.5 1)
                       (vector 1 1 0))
                      (bot-id bot)))
             (rotate (vector -90 0 90))
             (load-primitive "meshes/bot.obj")))
         ;; build carrying block
         (c (with-state
             (parent p)
             (translate (vector 0 0.25 -1))
             (build-cube))))
    ;; attach the code brick here
    (with-primitive 
     (bot-brick-id bot)
     (parent p))
    (with-primitive 
     p
     (apply-transform)
     (recalc-normals 1))
    (cheap-toon p -0.05 (vector 0 0 0))
    (with-primitive 
     p
     (recalc-normals 0))
    ;; hide the carry block
    (with-primitive c (hide 1))
    ;; create the main thing
    (make-bot-view (bot-id bot) p c)))

(define (make-bot-views l) l)
(define (bot-views-add bvs bv) (cons bv bvs))
(define (bot-views-find bvs id)
  (assq id bvs))

(define (bot-views-update bot-views bots bricks)
  ;; update them all
  (map
   (lambda (bot-view bot)
     (bot-view-update bot-view bot bricks))
   ;; add new bots
   (foldl
    (lambda (bot bot-views)
      (if (not (bot-views-find bot-views (bot-id bot)))
          (bot-views-add bot-views (bot-view-create bot))
          bot-views))
    bot-views
    (bots-list bots))         
   (bots-list bots)))

