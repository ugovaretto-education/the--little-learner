#lang racket

(define rank ;; tail recursive
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t acc)
    (cond
     ((number? t) acc)
     (else (ranked (car t) (add1 acc))))))


(define shape
  (λ (t)
    (cond
     ((scalar? t) (list))
     (else (cons (length t) (shapee (car t)))))))


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

(define vsum
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

(define uv/tensor-ref
  (lambda (t coord)
    (if (empty? (cdr coord))
        (vector-ref t (car coord))
        (uv/tensor-ref (vector-ref t (car coord)) (cdr coord)))))

(define uv/tensor-set!
  (lambda (t coord e)
    (if (empty? (cdr coord))
        (vector-set! t (car coord) e)
        (uv/tensor-set! (vector-ref t (car coord)) (cdr coord) e))))

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
        ((uv/tensor-foldl (cdr ts) f (uv/tensor-bin-op f acc (car ts)))))))

(provide uv/tensor-fold)
(define uv/tensor-fold
  (lambda (f ts)
    (uv-tensor-foldl (cdr ts) f (car ts))))



