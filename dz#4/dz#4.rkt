(load "test.scm")
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


         
                 

(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s #f)
(define r #f)
(define t #f)
(define c #f)

;; Приближённое значение числа Пи, рекомендуемое Архимедом.
;; См.: https://ru.wikipedia.org/wiki/Пи_(число)#История
;; Это рациональное число, оно точное (exact), с ним ошибок округления не бывает.
(define PI 22/7)

(define (perim f)
  (match f
    ((square a) (* 4 a))
    ((rectangle a b) (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r) (* 2 PI r))))

(define (scale f x)
  (match f
    ((square a) (square (* a x)))
    ((rectangle a b) (rectangle (* a x) (* b x)))
    ((triangle a b c) (triangle (* a x) (* b x) (* c x)))
    ((circle r) (circle (* r x)))))


(define-data the-list ((the-nil)
                       (the-cons head tail)))

(define (the-length xs)
  (match xs
    ((the-nil) 0)
    ((the-cons hd tl) (+ 1 (the-length tl)))))

(define (the-prod xs)
  (match xs
    ((the-nil) 1)
    ((the-cons hd tl)
     (match tl
       ((the-nil) hd)
       ((the-cons _1 _2) (* hd (the-prod tl)))))))

(define the-xs #f)


(define tests
  (list (test (figure? #t) #f)
        (test (figure? #f) #f)
        (test (figure? 'hello) #f)
        (test (figure? 888) #f)
        (test (figure? '()) #f)
        (test (figure? #()) #f)
        (test (figure? '(one two three)) #f)
        (test (figure? #(one two three)) #f)

        (test (begin (set! s (square 100))
                     (figure? s))
              #t)
        (test (begin (set! r (rectangle 100 200))
                     (figure? r))
              #t)
        (test (begin (set! t (triangle 300 400 500))
                     (figure? t))
              #t)
        (test (begin (set! c (circle 100))
                     (figure? t))
              #t)

        (test (the-list? s) #f)
        (test (the-list? r) #f)
        (test (the-list? t) #f)
        (test (the-list? c) #f)

        (test (perim s) 400)
        (test (perim r) 600)
        (test (perim t) 1200)
        (test (perim c) 4400/7)

        (test (perim (scale s 1/100)) 4)
        (test (perim (scale r 1/100)) 6)
        (test (perim (scale t 1/100)) 12)
        (test (perim (scale c 1/100)) 44/7)

        (test (the-list? #t) #f)
        (test (the-list? #f) #f)
        (test (the-list? 'hello) #f)
        (test (the-list? 888) #f)
        (test (the-list? '()) #f)
        (test (the-list? #()) #f)
        (test (the-list? '(one two three)) #f)
        (test (the-list? #(one two three)) #f)

        (test (begin (set! the-xs
                           (the-cons 7 (the-cons 11 (the-cons 13 (the-nil)))))
                     (the-list? the-xs))
              #t)
        (test (figure? the-xs) #f)

        (test (the-length the-xs) 3)
        (test (the-prod the-xs) 1001)))


(define **test-succeed-27326** (run-tests tests))
    
;;================================== ACHIEVMENTS ==================================;;



