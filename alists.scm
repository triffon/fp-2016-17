(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define al (make-alist (lambda (x) (* x x)) '(1 2 3 4 5 6 7)))

(define (keys al) (map car al))
(define (vals al) (map cdr al))

(define (del-assoc key al)
  (filter (lambda (kv) (not (eq? (car kv) key))) al))

(define (exists p? l)
  (not (null? (filter p? l))))

(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (my-assq key l) (search (lambda (kv) (and (eq? (car kv) key) kv)) l))

(define (forall p l)
  (not (search (lambda (x) (not (p x))) l)))