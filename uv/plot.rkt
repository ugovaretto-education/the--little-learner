(require plot)
(define vmin
  (λ (v)
    (vector-argmin (λ (x) x) v)))

(define vmax
  (λ (v)
    (vector-argmax (λ (x) x) v)))


(define scatter-plot
  (lambda (xs ys color)
    (let
        (
         (xm (- (vmin xs) 0.1))
         (xM (+ (vmax xs) 0.1))
         (ym (- (vmin ys) 0.1))
         (yM (+ (vmax ys) 0.1))
         )
    (plot
     (points
      (vector-map vector xs ys)
      #:color color
      #:x-min xm
      #:x-max xM
      #:y-min ym
      #:y-max yM
    )))))
