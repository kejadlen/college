(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ((member (first s1) s2) (union (rest s1) s2))
    (else (union (rest s1) (cons (first s1) s2)))))

(define (largest l)
  (apply max l))

(define (largest-smallest l)
  (list (apply max l) (apply min l)))

