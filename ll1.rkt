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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 3

(define line-xs #(2.0 1.0 4.0 3.0))
(define line-ys #(1.8 1.2 4.2 3.3))


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
               #:x-min xmin #:x-max xmax #:y-min ymin #:y-max ymax)
