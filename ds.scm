(map list '(1 2 3) '(4 5 6) '(7 8 9))

(define m '((1 2 3) (4 5 6) (7 8 9)))

(define (transpose m) (apply map list m))

