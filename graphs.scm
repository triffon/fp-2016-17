(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (forall p l)
  (not (search (lambda (x) (not (p x))) l)))

(define g '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)))

(define (vertices g) (map car g))

(define (children v g) (cdr (assq v g)))

(define (edge? u v g) (memq v (children u g)))

(define (map-children v f g) (map f (children v g)))

(define (search-child v f g) (search f (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (symmetric? g)
  (forall (lambda (u)
            (forall (lambda (v) (edge? v u g)) (children u g)))
            (vertices g)))

(define g2 '((1 2 3) (2 1) (3 1)))

(define (dfs-path u v g)
  (or (and (eq? u v) (list u))
      (cons#f u (search-child u (lambda (w) (dfs-path w v g)) g))))

(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((current (car path)))
      (cond ((eq? current v) path) ; ДА
            ((memq current (cdr path)) #f)  ; НЕ
            (else (search-child current (lambda (w) (dfs-search (cons w path))) g)))))
  (dfs-search (list u)))

(define (bfs-path u v g)
  (define (extend path)
    (map-children (car path) (lambda (v) (cons v path)) g))
  
  (define (acyclic? p)
    (not (memq (car p) (cdr p))))
  
  (define (extend-acyclic path) ; p -> [p]
    (filter acyclic? (extend path)))

  (define (target-path p) (and (eq? (car p) v) p))

  (define (extend-level level)          ; [p] -> [p]
    (apply append                       ; [[p]] -> [p]
           (map extend-acyclic level))) ; [p] -> [[p]]

  (define (bfs-level level)
     (or (search target-path level)
         (bfs-level (extend-level level))))

  (bfs-level (list (list u))))
