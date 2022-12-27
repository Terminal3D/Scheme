(load "test.scm")
(load "trace.scm")

(define ie (interaction-environment))


(define (derivative xs)

  
  #|(define (delem x)
    (cond ((number? x) 0)
          ((equal? x 'x) 1)
          ((equal? x '-x) -1)
          (else (write 'Error))))|#

  (define (delem dxs)
    
    (define expr (cond
                   ((pair? dxs) (car dxs))
                   ((and (not (pair? dxs)) (not (null? dxs))) dxs)
                   (else 'Error)))
                      
    (cond ((number? expr) 0)
          ((equal? expr '*) (if (<= (length (cdr dxs)) 2)
                                `(+ (* ,(delem (cadr dxs)) ,(caddr dxs)) (* ,(cadr dxs) ,(delem (caddr dxs))))
                                (delem (simpl-mult dxs))))
          
          ((equal? expr '/) `(/ (- (* ,(delem (cadr dxs)) ,(caddr dxs)) (* ,(cadr dxs) ,(delem (caddr dxs)))) (* ,(caddr dxs) ,(caddr dxs))))
          ((equal? expr 'x) 1)
          ((equal? expr '-x) -1)
          ((equal? expr 'expt) (if (number? (cadr dxs))
                                   `(* (expt ,(cadr dxs) ,(caddr dxs)) ,(log (cadr dxs)))
                                   `(* ,(delem (cadr dxs)) (* ,(caddr dxs) ,`(expt ,(cadr dxs) ,(- (caddr dxs) 1))))))
          ((equal? expr 'sin) `(* ,(delem (cadr dxs)) (cos ,(cadr dxs))))
          ((equal? expr 'cos) `(* -1 (* ,(delem (cadr dxs)) (sin ,(cadr dxs)))))
          ((equal? expr 'log) `(* ,(delem (cadr dxs)) (/ 1 ,(cadr dxs))))
          ((equal? expr 'exp) `(* ,(delem (cadr dxs)) (exp ,(cadr dxs))))
          ((equal? expr '+) `(+ ,(delem (cadr dxs)) ,(delem (caddr dxs))))
          ((equal? expr '-) `(- ,(delem (cadr dxs)) ,(delem (caddr dxs))))
          
          
          ))
  
  
  (delem xs))


(define (simpl xs)
  (if (<= (length (cdr xs)) 2)
      `(,(car xs) ,(cadr xs) ,(caddr xs))
      `(,(car xs) ,(cadr xs) ,(simpl (cons (car xs) (cddr xs))))))

(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (pair? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))



(define-syntax flatten
  (syntax-rules ()
    ((_ xs)
     (letrec ((loop (lambda (cxs)
                      (if (null? cxs)
                          '()
                          (if (pair? (car cxs))
                              (trace-ex (append (loop (car cxs)) (loop (cdr xs))))
                              (cons (car cxs) (loop (cdr cxs))))))))
       (loop xs)))))






  