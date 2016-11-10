(map list '(1 2 3) '(4 5 6) '(7 8 9))

(define m '((1 2 3) (4 5 6) (7 8 9)))

(define (transpose m) (apply map list m))

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

