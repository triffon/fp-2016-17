(define (atom? x) (not (pair? x)))

(define l '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(define (flatten l)
  (cond ((null? l) l)
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

;(define (reverse l)
;  (if (null? l) l
;      (append (reverse (cdr l)) (list (car l)))))


(define (deep-reverse l)
  (cond ((null? l) l)
        ((atom? l) l)
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define (deep-fold nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-fold nv term op (car l))
                  (deep-fold nv term op (cdr l))))))

(define (foldr nv op l)
  (if (null? l) nv
      (op (car l) (foldr nv op (cdr l)))))

(define (branch p? f g) (lambda (x) (if (p? x) (f x) (g x))))

(define (valid-atom? x) (and (not (pair? x)) (not (null? x))))

(define (deep-fold nv term op l)
  (foldr nv op (map (branch valid-atom? term (lambda (x) (deep-fold nv term op x))) l)))


(define (evali x) (eval x (interaction-environment)))


