;=============================================

(define (range a b d)
  (define (loop addout cura)
    (if (< cura b)
        (loop (append addout (cons cura '()))
              (+ cura d))
        addout))
  (loop '() a))

;;=============================================

(define-syntax when
  (syntax-rules ()
    ((_ cond? expr ...)
     (if cond?
         (begin
           expr ...)))))


(define-syntax unless
  (syntax-rules ()
    ((_ cond? expr ...)
     (if (not cond?)
         (begin
           expr ...)))))

;;=============================================
         
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs expr ...)
     (letrec ((loop (lambda (cxs)
                      (if (not(null? cxs))
                          (begin
                            ((lambda (x) expr ...) (car cxs))
                            (loop (cdr cxs))
                            )))))
       (loop xs)))
    ((for xs as x expr ...)
     (for x in xs expr ...))))

;;=============================================

(define-syntax while
  (syntax-rules ()
    ((while cond? expr ...)
     (let loop ()
       (when cond?
         expr ...
         (loop))))))

;;=============================================

(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr ...) until cond?)
     (let loop ()
       expr ...
       (if (not cond?)
           (loop)
           )))))

;;=============================================


            
