 (define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (list-tail l n)
  (if (= n 0) l
      (list-tail (cdr l) (- n 1))))

(define (list-ref l n)
   (car (list-tail l n)))

(define (member-gen x l e?)
  (cond ((null? l) #f)
        ((e? x (car l)) l)
        (else (member-gen x (cdr l) e?))))

(define (member x l) (member-gen x l equal?))

(define (memqv x l) (member-gen x l eqv?))

(define (memq x l) (member-gen x l eq?))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (reverse l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse-faster l)
  (define (iter l res)
    (if (null? l) res
        (iter (cdr l) (cons (car l) res))))
  (iter l '()))

(define (1+ x) (+ x 1))
(define (square x) (* x x))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (map f l)
  (foldr (lambda (h t) (cons (f h) t)) '() l))

(define (filter p? l)
  (foldr (lambda (h t) (if (p? h) (cons h t) t)) '() l))

(define (accumulate op nv a b term next)
  (foldr op nv (map term (collect a b next))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (reverse-faster l)
  (define (iter l res)
    (if (null? l) res
        (iter (cdr l) (cons (car l) res))))
  (iter l '()))


(define (reverse l)
  (foldl (lambda (x y) (cons y x)) '() l))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))