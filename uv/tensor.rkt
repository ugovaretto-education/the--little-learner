#lang racket

(define scalar? number?)

(provide rank)
(define rank ;; tail recursive
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t acc)
    (cond
     ((number? t) acc)
     (else (ranked (vector-ref t 0) (+ 1 acc))))))


(provide shape)
(define shape
  (λ (t)
    (cond
     ((= 1 (rank t)) (vector-length t))
     (else (cons (vector-length t) (shape (vector-ref t 0)))))))


(provide vector-reduce)
(define vector-reduce
  (lambda (f xs)
    (do ((i 1 (+ i 1))
         (a (vector-ref xs 0) (f (vector-ref xs i) a)))
        ((= i (vector-length xs)) a))))


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

(provide uv/make-tensor)
(define uv/make-tensor
  (lambda (shape (init-value 0.0))
    (let ((v (make-vector (car shape) init-value)))
      (do ((i 0 (+ i 1)))
          ((or (= i (vector-length v)) (empty? (cdr shape))) v)
        (vector-set! v i (uv/make-tensor (cdr shape) init-value))))))

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
  (lambda (f t1 t2)
      (if (number? (vector-ref t1 0))
          (reduce-vectors t1 t2 f)
          (do ((i 0 (+ 1 i))
               (t (make-vector (vector-length t1))))
              ((= i (vector-length t1)) t)
            (vector-set! t i
                         (uv/tensor-bin-op
                          f
                          (vector-ref t1 i) (vector-ref t2 i)))))))

(provide uv/tensor-foldl)
(define uv/tensor-foldl
  (lambda (ts f acc)
    (match ts
        ((list h) (uv/tensor-bin-op f h acc))
        (_ (uv/tensor-foldl (cdr ts) f (uv/tensor-bin-op f acc (car ts)))))))

(provide uv/tensor-fold)
(define uv/tensor-fold
  (lambda (f ts)
    (uv/tensor-foldl (cdr ts) f (car ts))))

(provide uv/tensor-mfold)
(define (uv/tensor-mfold . args)
  (uv/tensor-fold (car args) (cdr args)))


(provide uv/md-reduce)
(define uv/md-reduce
  (lambda (f t acc)
    (if (number? (vector-ref t 0))
        (f (vector-reduce f t) acc)
        (do ((i 1 (+ 1 i))
             (a (uv/md-reduce
                 f (vector-ref t 0) acc)
                (f (uv/md-reduce f (vector-ref t i) a))))
            ((= i (vector-length t)) a)))))

(provide uv/tensor-rand)
(define tensor-rand
  (lambda (t)
    (let ((coord (tensor-rand-coord (shape t) '())))
      (uv/tensor-ref t coord))))

(define tensor-rand-coord
  (lambda (s r)
    (if (empty? s)
        (r)
        (tensor-rand-coord (cdr s) (cons (rand (car s)) r)))))
 
