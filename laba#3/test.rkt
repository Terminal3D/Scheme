(load "trans.scm")
(define (ref . xs)
  (define unxs (cadr (trans (car xs))))
  (define type (car (trans (car xs))))
  ;  (if (>= (- (length unxs) 1) (cadr xs))
  (define (loop cxs)
    (if (< (length oxs) (cadr xs))
        (cons (car cxs) (cons 
        
  (if (member (+ (cadr xs) 1) unxs)
      (cons (caddr xs) (member (+ (cadr xs) 1) unxs))
      #f))
  