(load "test.scm")
(define ie (interaction-environment))


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






(define (simplify expr)
  (cond
    ((null? expr) '())
    ((not(pair? expr)) expr)
    ((pair? (car expr)) (cons (simplify (car expr)) (simplify (cdr expr))))
    ((equal? (car expr) '+)
     (let loop ((l-in (cdr expr)) (l-out '()))
       (cond
       ((null? l-in) (if (= (length l-out) 1)
                         (car l-out)
                         (cons '+ l-out)))
       ((equal? 0 (simplify (car l-in))) (loop (cdr l-in) l-out))
       (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
     ((equal? (car expr) '*)
      (let loop ((l-in (cdr expr)) (l-out '()))
        (cond
        ((null? l-in) (if (= (length l-out) 1)
                         (car l-out)
                         (cons '* l-out)))
        ((equal? 0 (simplify (car l-in))) 0)
        ((equal? 1 (simplify (car l-in))) (loop (cdr l-in) l-out))
        (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
     (else (cons (car expr) (simplify (cdr expr))))))
        
       
       
       


(define-syntax flatten
  (syntax-rules()
    ((_ expr ... ((nested ...) todo ...))
     '(expr ...)
         
        )))

#|
(define (f x y z)
  (flatten (((+ x) 3 (4 y) (((z)))) (7 y x 8))))  ;; (+ x 3 4 y z 7 y x 8)


(define tests
  (list (test (f 1 1 1) 27)
        (test (let ((a 5)
                    (b 7)
                    (c 1))
                (flatten ((* (((a) (b)) c))))) ;; (* a b c)
              35)))


(define **test-succeed-6694** (run-tests tests))
|#

(define (any xs)
  (and (not (null? xs))
       (or (car xs) (any (cdr xs)))))


(define (find-invalid expr)
  (cond ((not (pair? expr)) #f)
        ((equal? (car expr) '+)
         (if (member 0 (cdr expr))
             expr
             (any (map find-invalid (cdr expr)))))
        ((equal? (car expr) '*)
         (if (or (member 0 (cdr expr))
                 (member 1 (cdr expr)))
             expr
             (any (map find-invalid (cdr expr)))))
        (else (any (map find-invalid (cdr expr))))))


(define ie (interaction-environment))

(define-syntax simplify-test
  (syntax-rules ()
    ((simplify-test argument expected call-arg)
     (test (eval `(let ((x call-arg))
                    ,(simplify 'argument))
                 ie)
           (eval '(let ((x call-arg))
                    expected)
                 ie)))))

(define PI (* 4 (atan 1)))


(define tests
  (list (test (simplify 'x) 'x)
        (test (simplify '(* x 5)) '(* x 5))
        (test (simplify '(* 0 5)) 0)
        (test (simplify '(* x 1)) 'x)
        (test (simplify '(* x 0)) 0)
        (test (simplify '(* 1 5)) 5)
        (test (simplify '(+ x 5)) '(+ x 5))
        (test (simplify '(+ x 0)) 'x)
        (test (simplify '(+ 0 5)) 5)
        (test (find-invalid (simplify '(* (+ 2 x 0 (* 3 x 1))
                                          (+ 3 5 1 (* 1 0)))))
              #f)
        (test (find-invalid (simplify '(+ (* (+ 0 (* 1 x))
                                             (cos (* 2 x (+ 1 0))))
                                          (* (+ 1 (* 0 x))
                                             (cos (* 2 x (+ 1 x)))))))
              #f)
        (simplify-test (* (+ 3 x) (+ 0 0)) 0 5)
        (simplify-test (+ (* (- 1 (* 2 x)) (cos (* 2 x (- 1 x))))
                          (* (+ 1 (* 2 x)) (cos (* 2 x (+ 1 x)))))
                       (+ (* (- 1 (* 2 x)) (cos (* 2 x (- 1 x))))
                          (* (+ 1 (* 2 x)) (cos (* 2 x (+ 1 x)))))
                       10)
        (simplify-test (+ (* (+ 0 (* 1 x)) (cos (* 2 x (+ 1 0))))
                          (* (+ 1 (* 0 x)) (cos (* 2 x (+ 1 x)))))
                       (+ (* x (cos (* 2 x))) (cos (* 2 x (+ 1 x))))
                       10)))


(define **test-succeed-27006** (run-tests tests))



         
     
     
     

