(define t '(1 (2 () ())
   (3 (4 () ())
      (5 () ()))))

(define (make-tree root left right) (list root left right))
(define make-tree list)

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (tree? t)
  (or (null? t) (and (list? t) (= (length t) 3)
                     (tree? (left-tree t)) (tree? (right-tree t)))))

(define (depth t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth (left-tree t)) (depth (right-tree t))))))

(define (memq-tree x t)
  (cond ((null? t) #f) ; НЕ
        ((eq? x (root-tree t)) t) ; ДА
        (else (or (memq-tree x (left-tree t))
                  (memq-tree x (right-tree t))))))

(define (memq-tree x t)
  (and (not (null? t))
       (or (and (eq? x (root-tree t)) t)
           (memq-tree x (left-tree t))
           (memq-tree x (right-tree t)))))

(define (cons#f x t)
  (and t (cons x t)))

(define (path-tree x t)
  (cond ((null? t) #f)
        ((eq? x (root-tree t)) (list x))
        (else (cons#f (root-tree t) (or 
                                     (path-tree x (left-tree t))
                                     (path-tree x (right-tree t)))))))