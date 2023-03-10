(define call/cc call-with-current-continuation)
(define *env* #f)
(define ie (interaction-environment))


;;=============================================

(define (range a b d)
  (define (loop addout cura)
    (if (< cura b)
        (loop (append addout (cons cura '()))
              (+ cura d))
        addout))
  (loop '() a))

;;=============================================

(define (use-assertions) (call/cc (lambda (cc)
                                    (set! *env* cc))))
(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (if (eval expr ie)
         #t
         (begin
           (display "fail: ")
           (*env* (write 'expr)))))))

(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5)) 
(map 1/x '(-2 -1 0 1 2))
(newline)

;;=============================================


(define (load-data file)
  (call-with-input-file file
    (lambda (port) (read port))))

(define (save-data data file)
  (call-with-output-file file (lambda (port) (write data port)))
  )

(define data (load-data "input.txt"))

(define (line-counter file)
  (call-with-input-file file
    (lambda (port)
      (define a1 "")
      (define a2 "")
      (define (loop counter)
        (set! a1 a2)
        (set! a2 (read-char port))
        (if (eof-object? a2)
            counter
            (if (and (equal? a2 #\return) (not (equal? a1 #\newline)))
                (loop (+ counter 1))
                (loop counter))))
      (loop 0))))


;;=============================================

(define (trib-num n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    (else (+ (trib-num (- n 1)) (trib-num (- n 2)) (trib-num (- n 3))))))


(define (trib-memo n)
  (define memo (make-vector (+ n 1)))
  (define (loop num)
    (cond
      ((<= num 1) 0)
      ((= num 2) 1)
      (else (if (zero? (vector-ref memo num))
                (vector-set! memo num
                             (+ (loop (- num 1))
                                (loop (- num 2))
                                (loop (- num 3)))))
            (vector-ref memo num))))
  (loop n))


;;=============================================

(define-syntax my-if
  (syntax-rules ()
    ((_ condition t f)
     (let ((t-promised (delay t)) (f-promised (delay f)))
       (force (or (and condition t-promised) f-promised))))))

;;=============================================



(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name val) ...) expr ...)
     ((lambda (name ...) expr ...) val ...))))


(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr ...)
     (my-let () expr ...))
    ((my-let* ((name value)) expr ...)
     (my-let ((name value)) expr ...))
    ((my-let* ((name value) . other) expr ...)
     ((lambda (name) (my-let* other expr ...)) value)
     )
    )
  )


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

(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . expr)
     (begin
       (newline)
       (cout . expr)))
    ((cout << expr1)
     (display expr1))
    ((cout << expr1 . expr)
     (begin (display expr1)
            (cout . expr)))))

;;=============================================


            
