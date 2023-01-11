
(define ie (interaction-environment))



(define (memoized-factorial n)
  (let mem-fact ((cv 1) (cn n) (xs '()))
    (if (= 0 cn)
        (cadr (assoc 1 xs))
        (mem-fact  (* cv cn) (- cn 1) (cons (list cn cv) xs)))))

;;=============================================

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ head tail)
     (cons head (delay tail)))))

(define (lazy-car p)
  (if (pair? p)
      (car p)
      p))

(define (lazy-cdr p)
  (if (pair? p)
      (force (cdr p))))

(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs n)
  (if (= n 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- n 1))))

(define (naturals start)
  (lazy-cons start (naturals (+ 1 start))))

(define (factorial)
  (let loop ((n 1) (m 1))
    (lazy-cons (* n m) (loop (+ n 1) (* n m)))))

(define (lazy-factorial n)
  (if (= n 0)
      1
      (lazy-ref (factorial) (- n 1))))

;;=============================================

(define (read-words)
  (let loop ((lout '()) (word ""))
    (if (eof-object? (peek-char))
        (reverse (if (> (string-length word) 0)
                     (cons word lout)
                     lout))
        (let ((x (read-char)))
          (if (char-whitespace? x)
              (if (> (string-length word) 0)
                  (loop (cons word lout) "")
                  (loop lout ""))
              (loop lout (string-append word (string x))))))))

;;=============================================

(define-syntax define-struct
  (syntax-rules ()
    ((_ name (param ...))
     (begin
       (eval (list 'define (string->symbol (string-append "make-" (symbol->string 'name)))
                   (lambda (param ...)
                     (list (list 'type 'name) (list 'param param) ...))) ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'name) "?"))
                   (lambda (x)
                     (if (and (list? x) (> (length x) 1))
                         (if (pair? (car x))
                             (and (equal? 'name (cadr (assoc 'type x))))
                             #f)
                         #f))) ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'name)
                 "-" (symbol->string 'param))) (lambda (x) (cadr (assoc 'param x)))) ie) ...

       (eval (list 'define (string->symbol (string-append "set-" (symbol->string 'name)
                   "-" (symbol->string 'param) "!")) (lambda (item value)
                           (set-car! (cdr (assoc 'param (cdr item))) value))) ie) ...  
       ))))  
                   
;;=============================================

(define-syntax define-data
  (syntax-rules ()
    ((_ data_name ((name param ...) ...))
     (begin
       (eval (list 'define 'name (lambda (param ...)
                      (list (list 'a-type 'data_name) (list 'd-type 'name) param ...))) ie) ...
       (eval (list 'define (string->symbol (string-append (symbol->string 'data_name) "?"))
                   (lambda (x)
                       (if (and (list? x) (> (length x) 1))
                        (if (pair? (car x))
                          (and (equal? 'data_name (cadr (assoc 'a-type x))))
                          #f)
                      #f))) ie)))))


(define-syntax match
  (syntax-rules ()
    ((_ x ((name param ...) expr) ...)
     (cond
       ((equal? (cadadr x) 'name)
        (apply (lambda (param ...) expr) (cddr x))
        )
       ...
       (else x)))))


         
                 
