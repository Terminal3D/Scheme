(load "trace.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))

(define counter 0)
(define (next)
  (set! counter (+ counter 1))
  counter)

;;=================================

(load "test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0) 1)
    (else     1)))

(define the-tests
  (list 
   (test (signum -2) -1)
   (test (signum 0) 0)
   (test (signum 2) 1)
   (test (signum 100) 1)))

;(run-tests the-tests)

;(run-test (test (signum -50) 0))
   
;;=================================

(load "trans.scm")
(load "insert-at-pos.scm")


(define (ref . xs)
  (define unxs (cadr (trans (car xs))))
  (define type (car (trans (car xs))))

  ;(write type)
  (cond
    ((= (length xs) 2) (begin
                         (if (>= (- (length unxs) 1) (cadr xs))
                             (list-ref unxs (cadr xs))

                             #f)))
    ((= (length xs) 3) (begin
                         (if (equal? type '("string"))
                             (and (char? (caddr xs))                                 
                                 (and (>= (- (length unxs) 1) (cadr xs))
                                     (rtrans (cons type (cons (insert-at-pos (cadr xs) (caddr xs) unxs) '()))))
                                     )
                             (and (>= (- (length unxs) 1) (cadr xs))
                                 (rtrans (cons type (cons (insert-at-pos (cadr xs) (caddr xs) unxs) '())))
                                 ))))))
                                 
                                     
                         
                     
                        
;;=================================

(define (factorize expr)
  (define sign (ref expr 0))
  (define first-elem (ref (ref expr 1) 1))
  (define second-elem (ref (ref expr 2) 1))
  (define power (ref (ref expr 1) 2))
  (define (testout . lst)
    lst)
  (testout sign first-elem second-elem power)
  (cond 
    ((equal? sign '-) (if (= power 2)
                          `(* (- ,first-elem ,second-elem) (+ ,first-elem ,second-elem))
                          `(* (- ,first-elem ,second-elem) (+ (expt ,first-elem 2) (* ,first-elem ,second-elem) (expt ,second-elem 2)))))
    ((equal? sign '+) (if (= power 3)
                          `(* (+ ,first-elem ,second-elem) (+ (expt ,first-elem 2) (- (* ,first-elem ,second-elem)) (expt ,second-elem 2)))
                          #f))
    (else #f)))
      
                              
         
                          
  














