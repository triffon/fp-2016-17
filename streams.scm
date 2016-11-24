(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

(define bigpromise (delay (* 0 (fact 16000))))

(define the-empty-stream '())

; !!! (define (cons-stream h t) (cons h (delay t)))
(define-syntax cons-stream (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define empty-stream? null?)

(define s2 (cons-stream 2 (cons-stream (fact 16000) the-empty-stream)))

(define-syntax mydelay (syntax-rules () ((delay x) (lambda () x))))
(define (myforce x) (x))

(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (first n s)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (first (- n 1) (tail s)))))

(define (search-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (search-stream p? (tail s)))))

(define (from n)
  (cons-stream n (from (+ n 1))))

; (define nats (from 0))

(define (generate-fibs a b)
  (cons-stream a (generate-fibs b (+ a b))))

; (define fibs (generate-fibs 0 1))

(define (map-stream f s)
  (if (empty-stream? s) s
      (cons-stream (f (head s)) (map-stream f (tail s)))))

(define (square x) (* x x))

(define (filter-stream p? s)
  (cond ((empty-stream? s) s)
        ((p? (head s)) (cons-stream (head s) (filter-stream p? (tail s))))
        (else (filter-stream p? (tail s)))))

(define (zip-streams op s1 s2)
  (if (or (empty-stream? s1) (empty-stream? s2)) '()
      (cons-stream (op (head s1) (head s2))
                   (zip-streams op (tail s1) (tail s2)))))

(define ones (cons-stream 1 ones))

; (define nats (zip-stream - (tail nats) ones))
(define nats (cons-stream 0 (zip-streams + nats ones)))

(define fibs (cons-stream 0
                          (cons-stream 1
                                       (zip-streams + fibs (tail fibs)))))

(define (notdivides d)
  (lambda (n) (> (remainder n d) 0)))

(define (sieve s)
  (cons-stream (head s)
               (sieve (filter-stream (notdivides (head s)) (tail s)))))

(define (sieve2 s)
  (cons-stream (head s)
               (filter-stream (notdivides (head s)) (sieve2 (tail s)))))


(define primes (sieve (from 2)))

(define primes2 (sieve2 (from 2)))

(define (stream-ref n s)
  (if (= n 1) (head s)
      (stream-ref (- n 1) (tail s))))