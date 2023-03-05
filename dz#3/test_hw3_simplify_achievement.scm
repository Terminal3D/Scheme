(load "fas.rkt")
(load "test.scm")


(define (simplify expr)
  (define (loop ex)
    (cond ((null? ex) '())
          ((or (symbol? ex) (number? ex)) ex)
          ((pair? (car ex)) (cons (loop (car ex)) (loop (cdr ex))))
          ((eqv? (car ex) '*)
           (let ((sm (simpl-mult (cdr ex))))
             (if (pair? sm)
                 (cons '* sm)
                 sm)))
          ((eqv? (car ex) '+)
           (let ((sa (simpl-add (cdr ex))))
             (if (pair? sa)
                 (cons '+ sm)
                 sa)))
          (else (cons (car ex) (loop (cdr ex))))))
  (loop expr))
  

(define (simpl-add expr)
  (if (> (length expr) 1)
      (let ((res (simplify (car expr))))
        (if (or (equal? res '(0)) (equal? res 0))
            (simpl-add (cdr expr))
            (let ((res2 (simpl-add (cdr expr))))
              (if (equal? res2 '(0))
                  res
                  (cons res res2)))))
      (simplify expr)))
    
(define (simpl-mult expr)
  (if (> (length expr) 1)
      (let ((res (simplify (car expr))))
        (if (or (equal? res '(0)) (equal? res 0))
            0
            (if (or (equal? res '(1)) (equal? res 1))
                (let ((res2 (simpl-mult (cdr expr))))
                  (cond
                    ((equal? res2 0)
                     0)
                    ((equal? res2 1)
                     res)
                    (else (cons res res2)))))))
      (simplify expr)))

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
