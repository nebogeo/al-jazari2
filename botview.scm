(load "bots.scm")

(define (make-bot-view id prim) (cons id prim))
(define (bot-view-id bv) (car bv))
(define (bot-view-prim bv) (cdr bv))

(define (bot-view-update bv bot)
  (with-primitive 
   (bot-view-prim bv)
   (identity)
   (translate (vector 0.5 0.5 0.5))
   (translate (bot-pos bot)))
  bv)

(define (bot-view-create bot)
  (make-bot-view
   (bot-id bot)
   (with-state
    (build-cube))))

(define (make-bot-views l) l)
(define (bot-views-add bvs bv) (cons bv bvs))
(define (bot-views-find bvs id)
  (assq id bvs))

(define (bot-views-update bot-views bots)
  ;; update them all
  (map
   (lambda (bot-view bot)
     (bot-view-update bot-view bot))
   ;; add new bots
   (foldl
    (lambda (bot bot-views)
      (if (not (bot-views-find bot-views (bot-id bot)))
          (bot-views-add bot-views (bot-view-create bot))
          bot-views))
    bot-views
    (bots-list bots))         
   (bots-list bots)))

