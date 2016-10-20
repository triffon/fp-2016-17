(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibi n)
  (define (iter i fi fi-1)
    (if (= n i) fi
        (iter (+ i 1) (+ fi fi-1) fi)))
  (iter 1 1 0))

(define (fixed-point? f x) (= (f x) x))

(define (sum a b term next)
  (if (> a b) 0 (+ (term a) (sum (next a) b term next))))