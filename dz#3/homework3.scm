(define (make-bin expr)
  (define op (car expr))
  (let loop ((in (cdr expr)))
    (cond
      ((null? in) '())
      ((<= (length in) 2) (cons op in))
      (else `(,op ,(car in) ,(loop (cdr in)))))))
  

(define (derivative expr)
  (if (null? expr)
      '()
      (let ((head (if (pair? expr)
                      (car expr)
                      expr)))
        (cond
          ((equal? head 'x) 1)
          ((number? head) 0)
          ((equal? head '+) `(+ ,(derivative (cadr (make-bin expr))) ,(derivative (caddr (make-bin expr)))))
          ((equal? head '-) (if (equal? (length expr) 2)
                                `(- ,(derivative (cadr expr)))
                                `(- ,(derivative (cadr expr)) ,(derivative (caddr expr)))))
          ((equal? head '*) `(+ (* ,(derivative (cadr (make-bin expr))) ,(caddr (make-bin expr)))
                                (* ,(derivative (caddr (make-bin expr))) ,(cadr (make-bin expr)))))
          ((equal? head '/) `(/ (- (* ,(derivative (cadr expr)) ,(caddr expr))
                                   (* ,(derivative (caddr expr)) ,(cadr expr)))
                                (* ,(caddr expr) ,(caddr expr))))
          ((equal? head 'cos) `(* ,(derivative (cadr expr)) (* -1 (sin ,(cadr expr)))))
          ((equal? head 'sin) `(* ,(derivative (cadr expr)) (cos ,(cadr expr))))
          ((equal? head 'log) `(* ,(derivative (cadr expr)) (/ 1 ,(cadr expr))))
          ((equal? head 'exp) `(* ,(derivative (cadr expr)) (exp ,(cadr expr))))
          ((equal? head 'expt) (if (number? (caddr expr))
                                   `(* ,(derivative (cadr expr))
                                       (* ,(caddr expr) (expt ,(cadr expr) ,(- (caddr expr) 1))))
                                   `(* ,(derivative (caddr expr)) (* ,expr (log ,(cadr expr))))))
        

          ))))



(define-syntax flatten
  (syntax-rules ()
    ((_ ((nest ...) others ...) expr ...)
     (flatten (nest ... others ...) expr ...))
    ((_ (head tail ...) expr ...)
     (flatten (tail ...) expr ... head))
    ((_ () expr ...)
     (expr ...))))

(define-syntax take-pointer
  (syntax-rules ()
    ((_ (first (nested ...) others ...))
     (take-pointer (first nested ... others ...)))
    ((_ (first others ...))
     (if (equal? 'first 'x)
         first
         (take-pointer (others ...))))
    ((_ smth ...)
     #f)))



(define-syntax mderivative
  (syntax-rules ()
    ((_ expr)
     (eval `((lambda (x) 
              ,(derivative 'expr)) ,(take-pointer expr)) (interaction-environment)))))

(define (simplify expr)
  (cond
    ((null? expr) '())
    ((not(pair? expr)) expr)
    ((pair? (car expr)) (cons (simplify (car expr)) (simplify (cdr expr))))
    ((equal? (car expr) '*)
     (let loop ((l-in (cdr expr)) (l-out '()))
       (cond
         ((null? l-in) (if (= (length l-out) 1)
                           (car l-out)
                           (cons '* l-out)))
         ((equal? 0 (simplify (car l-in))) 0)
         ((equal? 1 (simplify (car l-in))) (loop (cdr l-in) l-out))
         (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
    ((equal? (car expr) '+)
     (let loop ((l-in (cdr expr)) (l-out '()))
       (cond
         ((null? l-in) (if (= (length l-out) 1)
                           (car l-out)
                           (cons '+ l-out)))
         ((equal? 0 (simplify (car l-in))) (loop (cdr l-in) l-out))
         (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
    (else (cons (car expr) (simplify (cdr expr))))))


