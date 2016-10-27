(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))
(define (id x) x)

(define (p n x)
  (define (term i) (* (- (1+ n) i) (expt x i)))
   (accumulate + 0 0 n term 1+))

(define (p2 n x)
  (define (op a b) (+ (* a x) b))
  (accumulate op 0 1 (1+ n) id 1+))

(define (p3 n x)
  (define (op a b) (+ (* b x) a))
  (accumulate op 0 1 (1+ n) id 1+))

(define (p4 n x)
  (define (op a b) (+ (* b x) a))
  (define (term i) (- (1+ n) i))
  (accumulate op 0 1 (1+ n) term 1+))

(define (sum a b)
  (if (> a b) 0
      (+ a (sum (1+ a) b))))

(define (sum-i a b)
  (define (iter i r)
    (if (> i b) r
        (iter (1+ i) (+ r i))))
  (iter a 0))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (p5 n x)
  (define (op a b) (+ (* a x) b))
  (accumulate-i op 0 0 (1+ n) id 1+))

(define (fact n)
  (accumulate * 1 1 n id 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (expt1 x n)
  (accumulate + 0 0 n
              (lambda (i)
                (/
                   (accumulate * 1 1 i (lambda (j) x) 1+)
                   (accumulate * 1 1 i id 1+)))
              1+))

(define (exists? p a b)
  (accumulate (lambda (x y) (or x y)) #f a b p 1+))

(define (prime n)
  (not (exists? (lambda (d) (= (remainder n d) 0)) 2 (- n 1))))

(define (twice f)
  (lambda (x) (f (f x))))

(define (square x) (* x x))
(define mystery ((twice twice) square))

; (mystery 2)

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (compose f g)
   (lambda (x) (f (g x))))

(define (repeated n f)
  (if (= n 0) id
      (lambda (x) (f ((repeated (- n 1) f) x)))))

(define (repeated n f)
  (if (= n 0) id
      (compose f (repeated (- n 1) f))))

(define (repeated n f)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (pow x n)
  (accumulate *        1 1 n (lambda (i) x) 1+))


(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated n (lambda (g) (derive g dx))) f))

(define (derive-n-op n dx)
  (accumulate compose id 1 n
              (lambda (i)
                 (lambda (g) (derive g dx)))
              1+))


(define my-#t (lambda (x y) x))
(define my-#f (lambda (x y) y))
(define (my-if b x y) (b x y))

(define Y (lambda (f)
((lambda (x) (f (lambda (n) ((x x) n))))
(lambda (x) (f (lambda (n) ((x x) n)))))))
(define fact-body (lambda (f)
(lambda (n) (if (= n 0) 1
(* n (f (- n 1)))))))
(define fact (Y fact-body))

(define pow2-body (lambda (f)
                   (lambda (n)
                     (if (= n 0) 1
                         (* 2 (f (- n 1)))))))