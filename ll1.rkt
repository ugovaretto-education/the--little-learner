;; (require malt plot)


;; (define p
;;   (λ (m)
;;   (print m)
;;   (newline)))

;; (define (mmin a b)
;;   (if (< a b)
;;       a
;;       b))

;; (define (loop from to step)
;;   (do ((start from (+ start step)))
;;       ((>= start to) start)
;;     (print start)
;;     (newline)))

;; (define make-list
;;   (case-lambda
;;     [(n) (make-list n #f)]
;;     [(n x)
;;      (do ([n n (- n 1)] [ls '() (cons x ls)])
;;          ((zero? n) ls))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility compile before (require malt) !!!
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1

(define line1
  (λ (x)
    (λ (w b)
      (+ (* w x) b))))


(define line1-eq
  (λ (w b)
    (λ (x)
      (+ (* w x) b))))

;; (require plot)
;; (plot (function (line-eq '(0.5 -1)) (- 2) 2 #:label "y = line-eq(x)"))
;; applicaiton x -> y: (vector-map (line-eq 0.5 1) #(1 2 3 4))
;; Theta: θ 003B8

;; (require plot)
;; (define xs '(0 1 2 3 4 5))
;; (define ys '(0 1 4 9 16 25))
;; (plot (points (map vector xs ys) #:color 'red))
;; use vector-map if xs ys are vectors



(define line
  (λ (x)
    (λ (θ)
      (+ (* (car θ) x) (cadr θ)))))

(define line-eq
  (λ (θ)
    (λ (x)
      (+
       (* (car θ) x)
       (cadr θ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2

(define add1
  (λ (x)
    (set! x (+ 1 x))
    x))

(define rank1 ;; no tail recursion
  (λ (t)
    (cond
     ((scalar? t) 0)
     (else
      (+ 1 (rank1 (car t)))))))

(define rank ;; tail recursive
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t acc)
    (cond
     ((scalar? t) acc)
     (else (rankedd (car t) (add1 acc))))))


(define shape
  (λ (t)
    (cond
     ((scalar? t) (list))
     (else (cons (length t) (shapee (car t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interlude I

(define sum
  (λ (t)
    (summed t 0)))

(define summed
  (λ (t acc)
    (cond
     ((empty? t) acc)
     (else
      (summed (cdr t) (+ acc (car t)))))))

(define sum
  (λ (t)
    (vsummed t (- (vector-length t) 1) 0.0)))

(define vsummed
  (λ (t i acc)
    (cond
     ((= i 0) (+ acc (vector-ref t 0)))
     (else
      (vsummed t (- i 1) (+ acc (vector-ref t i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 3

(define line-xs #(2.0 1.0 4.0 3.0))
(define line-ys #(1.8 1.2 4.2 3.3))

(define reduce-vectors
  (lambda (xs ys f)
      (do ((i 0 (+ i 1))
           (zs (make-vector (vector-length xs) 0.0)))
          ((= i (vector-length zs)) zs)
        (vector-set! zs i (f (vector-ref xs i) (vector-ref ys i))))))

(define apply-element
  (lambda (vs i f acc)
      (cond
       ((empty? vs) acc)
       (else
        (apply-element (cdr vs) i f (f (vector-ref (car vs) i) acc))))))

(define symbol->function
  (lambda (f)
    (if (symbol? f)
        (eval f)
        f)))

(define reduce-multi-vectors
  (lambda (vs f)
    (do ((i 0 (+ i 1))
         (f (symbol->function f))
         (zs (make-vector (vector-length (car vs)) 0.0)))
        ((= i (vector-length zs)) zs)
      (vector-set! zs i
                   (apply-element (cdr vs) i f (vector-ref (car vs) 0))))))

(define (+t . args)
  (reduce-multi-vectors args +))

(define (-t . args)
  (reduce-multi-vectors args -))


;; call as (vectors-reduce )
(define (vectors-reduce  . args)
  (reduce-multi-vectors (cdr args) (car args)))

(define sum
  (λ (t)
    (vsummed t (- (vector-length t) 1) 0.0)))

(define vsummed
  (λ (t i acc)
    (cond
     ((= i 0) (+ acc (vector-ref t 0)))
     (else
      (vsummed t (- i 1) (+ acc (vector-ref t i)))))))


(define uv/make-tensor
  (lambda (shape (init-value 0.0))
    (let ((v (make-vector (car shape) init-value)))
      (do ((i 0 (+ i 1)))
          ((or (= i (vector-length v)) (empty? (cdr shape))) v)
        (vector-set! v i (make-tensor (cdr shape) init-value))))))

(define uv/tensor-get
  (lambda (t coord)
    (if (empty? (cdr coord))
        (vector-ref t (car coord))
        (uv/tensor-get (vector-ref t (car coord)) (cdr coord)))))

(define uv/tensor-set
  (lambda (t coord e)
    (if (empty? (cdr coord))
        (vector-set! t (car coord) e)
        (uv/tensor-set (vector-ref t (car coord)) (cdr coord) e))))

(define uv/tensor-bin-op
  (lambda (f t1 t2 t)
    (if (number? (vector-ref t1 0))
        (set! t (reduce-vectors t1 t2 f))
        (do ((i 0 (+ 1 i))
             (t (make-vector (vector-length t1))))
            ((= i (vector-length t1)))
          (uv/tensor-bin-op
           f (vector-ref t1 i) (vector-ref t2 i) (vector-ref t i))))))

(define uv/tensor-foldl
  (lambda (ts f acc)
    (if (empty? ts)
        (acc)
        (let (t (make-tensor (shape (car ts))))
          (uv/tensor-fold (cdr ts) f (uv/tensor-bin-op f acc (car ts)) t)
          t)))

(define line-eq
  (λ (θ)
    (λ (x)
      (+
       (* (car θ) x)
       (cadr θ)))))


(define line
  (lambda (xs)
    (lambda (theta)
      (vector-map (line-eq theta) xs))))

(define sqr-t
  (lambda (xs)
    (if (number? xs)
        (sqr xs)
        (vector-map sqr xs))))

(define uv-l2-loss
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (let ((pred-ys ((target-fun xs) params)))
          (sum
           (sqr-t
            (-t ys pred-ys))))))))


;; t0 = t0 - learning-rate * rate-of-change

(define line-loss-theta-0
  (lambda (theta-1)
    (lambda (xs ys)
      (lambda (x)
        (((uv-l2-loss line) xs ys (list x theta-1)))))))

(define line-loss-theta
  (lambda (xs ys theta1)
    (lambda(x)
      (((uv-l2-loss line) xs ys) (list x theta1)))))

(require plot)
(define plot-loss-theta-0
  (lambda (xs ys theta-1 xmin xmax ymin ymax)
    (plot
     (function ((line-loss-theta theta-1) xs ys) xmin xmax
               #:x-min xmin #:x-max xmax #:y-min ymin #:y-max ymax))))

