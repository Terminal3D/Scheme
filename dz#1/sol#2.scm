(define (square-equation a b c)
  (define ans
    (let ((discr (- (* b b) (* 4 a c))))
      (if (>= discr 0)
          (let ((ans1 (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
                (ans2 (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))
            (list ans1 ans2))
          '())))
  (cond
    ((null? ans) '())
    ((= (car ans) (cadr ans)) (list (car ans)))
    ((> (car ans) (cadr ans)) (list (cadr ans) (car ans)))
    (else ans)))