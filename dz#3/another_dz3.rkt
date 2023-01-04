(load "test.scm")
(load "trace.scm")

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
                                   `(* ,(derivative (cadr expr)) (* ,(caddr expr) (expt ,(cadr expr) ,(- (caddr expr) 1))))
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
    ((_ (first others ...))
     (if (equal? 'first 'x)
         first
         (take-pointer (others ...))))
    ((_ ())
     #f)))

(define-syntax mderivative
  (syntax-rules ()
    ((_ expr)
     (eval `(let ((x ,(take-pointer expr)))
              ,(derivative 'expr)) (interaction-environment)))))

(define (simplify exp)
  (cond
    (trace-ex exp)
    ((null? exp) '())
    ((not(list? exp)) exp)
    ((list? (car exp)) (cons (simplify (car exp)) (simplify (cdr exp))))
    ((and (eq? (car exp) '*))
     (let loop ((args (cdr exp)) (out '()))
       (cond
         ((null? args) (if (= (length out) 1)
                           (car out)
                           (cons '* out)))
         ((equal? 0 (simplify (car args))) 0)
         ((equal? 1 (simplify (car args))) (loop (cdr args) out))
         (else (loop (cdr args) (append out (list (simplify (car args)))))))))
    ((and (eq? (car exp) '+))
     (let loop ((args (cdr exp)) (out '()))
       (cond
         ((null? args) (if (= (length out) 1)
                           (car out)
                           (cons '+ out)))
         ((equal? 0 (simplify (car args))) (loop (cdr args) out))
         (else (loop (cdr args) (append out (list (simplify (car args)))))))))
    (else exp)))




     

(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ПРОВЕРЯЕТ simplify=-\n\n")

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
