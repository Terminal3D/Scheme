(define (insert-at-pos pos elem lst)
  (if (null? lst)
      (list elem)
      (if (= 0 pos)
          (cons elem lst)
          (cons (car lst) 
                (insert-at-pos (- pos 1) elem (cdr lst))))))

