(define (derivative xs)
  (cond
    ((not (list? xs)) (derivative (list xs)))
    ((equal? (car xs) 'x) 1)
    ((number? (car xs)) 0)
    ((equal? (car xs) '+)
     (cons '+
           (let loop ((in (cdr xs)))
             (cond
               ((null? in) '())
               (else (cons (derivative (car in)) (loop (cdr in))))))))
    ((equal? (car xs) '-) (if (= (length xs) 2)
                              `(- ,(derivative (cadr xs)))
                              `(- ,(derivative (cadr xs)) ,(derivative (caddr xs)))))
    ((equal? (car xs) '*)
     (if (= (length xs) 3)
         `(+ (* ,(derivative (cadr xs)) ,(caddr xs))
             (* ,(cadr xs) ,(derivative (caddr xs))))
         (derivative
         (let loop ((in (cdr xs)))
           (cond
             ((= (length in) 1) (car in))
             (else `(* ,(car in) ,(loop (cdr in)))))))))
     ((equal? (car xs) '/) `(/ (- (* ,(derivative (cadr xs)) ,(caddr xs))
                                  (* ,(cadr xs) ,(derivative (caddr xs))))
                               (expt ,(caddr xs) 2)))
     ((equal? (car xs) 'log) `(* ,(derivative (cadr xs)) (/ ,(cadr xs))))
     ((equal? (car xs) 'sin) `(* ,(derivative (cadr xs)) (cos ,(cadr xs))))
     ((equal? (car xs) 'cos) `(- (* ,(derivative (cadr xs)) (sin ,(cadr xs)))))
     ((equal? (car xs) 'exp) `(* ,(derivative (cadr xs)) (exp ,(cadr xs))))
     ((and (equal? (car xs) 'expt) (number? (cadr xs))) `(* ,(derivative (caddr xs))
                                                            (log ,(cadr xs)) ,xs))
     ((equal? (car xs) 'expt) `(* ,(derivative (cadr xs)) ,(caddr xs)
                                  (expt ,(cadr xs) ,(- (caddr xs) 1))))))

    
    