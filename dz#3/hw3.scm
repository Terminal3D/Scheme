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




     

(define ie (interaction-environment))

(define-syntax mderivative-test
  (syntax-rules ()
    ((mderivative-test argument expected call-arg)
     (test (eval `(let* ((f (lambda (x) (mderivative argument)))
                         (x call-arg))
                    (f x))
                 ie)
           (eval `(let ((x call-arg))
                    expected)
                 ie)))))

(define PI (* 4 (atan 1)))

(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ВЫПОЛНЯЕТ ТЕСТЫ ДЛЯ ПРОИЗВОДНОЙ=-\n\n")


(define (der-cube x)
  (mderivative (expt x 3)))


(define tests
  (list (test (der-cube 10) 300)
        (mderivative-test 2 0 10)
        (mderivative-test x 1 10)
        (mderivative-test (- x) -1 10)
        (mderivative-test (* 1 x) 1 10)
        (mderivative-test (* -1 x) -1 10)
        (mderivative-test (* -4 x) -4 10)
        (mderivative-test (* x 10) 10 10)
        (mderivative-test (- (* 2 x) 3) 2 10)
        (mderivative-test (expt x 10) (* 10 (expt x 9)) 10)
        (mderivative-test (* 2 (expt x 5)) (* 10 (expt x 4)) 10)
        (mderivative-test (expt x -2) (* -2 (expt x -3)) 10)
        (mderivative-test (expt 5 x) (* (log 5) (expt 5 x)) 10)
        (mderivative-test (cos x) (- (sin x)) (/ PI 2))
        (mderivative-test (sin x) (cos x) 0)
        (mderivative-test (exp x) (exp x) 10)
        (mderivative-test (* 2 (exp x)) (* 2 (exp x)) 10)
        (mderivative-test (* 2 (exp (* 2 x))) (* 4 (exp (* 2 x))) 10)
        (mderivative-test (log x) (/ 1 x) 7)
        (mderivative-test (* (log x) 3) (/ 3 x) 7)
        (mderivative-test (+ (expt x 3) (* x x)) (+ (* 3 x x) (* 2 x)) 10)
        (mderivative-test (- (* 2 (expt x 3)) (* 2 (expt x 2)))
                          (- (* 6 x x) (* 4 x))
                          10)
        (mderivative-test (/ 3 x) (/ -3 (* x x)) 7)
        (mderivative-test (/ 3 (* 2 (expt x 2))) (/ -3 (expt x 3)) 7)
        (mderivative-test (* 2 (sin x) (cos x))
                          (* 2 (cos (* 2 x)))
                          (/ PI 3))
        (mderivative-test (* 2 (exp x) (sin x) (cos x))
                          (* (exp x) (+ (* 2 (cos (* 2 x))) (sin (* 2 x))))
                          0)
        (mderivative-test (cos (* 2 (expt x 2)))
                          (* -4 x (sin (* 2 (expt x 2))))
                          5)
        (mderivative-test (sin (log (expt x 2)))
                          (/ (* 2 (cos (log (expt x 2)))) x)
                          15)
        (mderivative-test (+ (sin (+ x x)) (cos (* x 2 x)))
                          (+ (* 2 (cos (* 2 x)))
                             (* -4 (sin (* x 2 x)) x))
                          10)
        (mderivative-test (* (sin (+ x x)) (cos (* x 2 x)))
                          (+ (* (- 1 (* 2 x)) (cos (* 2 x (- 1 x))))
                             (* (+ 1 (* 2 x)) (cos (* 2 x (+ 1 x)))))
                          5)))

; Если будут проблемы, связанные с округлением, пишите, тест уточню.

(define **test-succeed-29553** (run-tests tests))
