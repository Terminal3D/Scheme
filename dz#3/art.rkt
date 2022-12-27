
(define (derivative xs)

  
  (define (simpl-mult xs)
    (if (<= (length (cdr xs)) 2)
        `(,(car xs) ,(cadr xs) ,(caddr xs))
        `(,(car xs) ,(cadr xs) ,(simpl-mult (cons (car xs) (cddr xs))))))

  (define (delem dxs)
    
    (define expr (cond
                   ((pair? dxs) (car dxs))
                   ((and (not (pair? dxs)) (not (null? dxs))) dxs)
                   (else 'Error)))
                      
    (cond ((number? expr) 0)
          ((equal? expr '*) (if (<= (length (cdr dxs)) 2)
                                `(+ (* ,(delem (cadr dxs)) ,(caddr dxs)) (* ,(cadr dxs) ,(delem (caddr dxs))))
                                (delem (simpl-mult dxs))))    
          ((equal? expr '/) `(/ (- (* ,(delem (cadr dxs)) ,(caddr dxs))
                                   (* ,(cadr dxs) ,(delem (caddr dxs)))) (* ,(caddr dxs) ,(caddr dxs))))
          ((equal? expr 'x) 1)
          ((equal? expr '-x) -1)
          ((equal? expr 'expt) (if (number? (cadr dxs))
                                   `(* (expt ,(cadr dxs) ,(caddr dxs)) ,(log (cadr dxs)))
                                   `(* ,(delem (cadr dxs))
                                       (* ,(caddr dxs) ,`(expt ,(cadr dxs) ,(- (caddr dxs) 1))))))
          ((equal? expr 'sin) `(* ,(delem (cadr dxs)) (cos ,(cadr dxs))))
          ((equal? expr 'cos) `(* -1 (* ,(delem (cadr dxs)) (sin ,(cadr dxs)))))
          ((equal? expr 'log) `(* ,(delem (cadr dxs)) (/ 1 ,(cadr dxs))))
          ((equal? expr 'exp) `(* ,(delem (cadr dxs)) (exp ,(cadr dxs))))
          ((equal? expr '+) (if (<= (length (cdr dxs)) 2)
                                `(+ ,(delem (cadr dxs)) ,(delem (caddr dxs)))
                                (delem (simpl-mult dxs))))
          ((equal? expr '-) (if (<= (length (cdr dxs)) 2)
                                (if (= (length (cdr dxs)) 1)
                                    `(- ,(delem (cadr dxs)))
                                    `(- ,(delem (cadr dxs)) ,(delem (caddr dxs))))
                                (delem (simpl-mult dxs))))
          
          
          ))
  
  
  (delem xs))