;;Александр Владимирович, я не знал, что ачивки нельзя досдать, после того, как
;;сдал основную программу. Можно будет вам как-нибудь очно или на сервер
;;сдать ачивки для 3-ей и 4-ой лабораторных работ?

(load "trace.scm")



(define (my-flatten xs)
  (let loop ((c-list xs) (l-out '()))
    (if (not(null? c-list))
        (if (list? c-list)
            (loop (car c-list) (loop (cdr c-list) l-out))
            (cons c-list l-out))
        l-out)))
            
(define (whitespace? x)
  (and (char? x) (char-whitespace? x)))     

(define (length-trim-right xs)
  (let loop ((c-list xs) (index 0) (stack 0))
    (if (null? c-list)
        index
        (let ((head (car c-list)))
          (cond
            ((not(whitespace? head)) (if (zero? stack)
                                         (loop (cdr c-list) (+ index 1) stack)
                                         (loop (cdr c-list) (+ index 1 stack) 0)))
            ((whitespace? head) (loop (cdr c-list) index (+ stack 1))))))))


(define (list-trim-right xs)
  (define max-index (length-trim-right xs))
  (let loop ((c-list xs) (index 0))
    (if (= index max-index)
        '()
        (cons (car c-list) (loop (cdr c-list) (+ index 1))))))

(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (if (= (length xs) 1)
          (car xs)
          '())
      (op (car xs) (my-fold-right op (cdr xs)))))

(define (o . xs)
  (lambda (a)
    (my-fold-right
     (lambda (op x)
       (op x))
     (append xs (list a)))))


(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name val)) expr)
     ((lambda (name) expr) val))
    ((my-let ((name val) . xs) expr)
     ((lambda (name) (my-let xs expr)) val))))


(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr)
     (my-let () expr))
    ((my-let* ((name value)) expr)
     (my-let ((name value)) expr))
    ((my-let* ((name value) . other) expr)
     ((lambda (name) (my-let* other expr)) value)
     )
    )
  )


(define (simplify expr)
  (cond
    ((null? expr) '())
    ((not(pair? expr)) expr)
    ((pair? (car expr)) (cons (simplify (car expr)) (simplify (cdr expr))))
    ((equal? (car expr) '+)
     (let loop ((l-in (cdr expr)) (l-out '()))
       (cond
       ((null? l-in) (if (= (length l-out) 1)
                         (car l-out)
                         (cons '+ l-out)))
       ((equal? 0 (simplify (car l-in))) (loop (cdr l-in) l-out))
       (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
     ((equal? (car expr) '*)
      (let loop ((l-in (cdr expr)) (l-out '()))
        (cond
        ((null? l-in) (if (= (length l-out) 1)
                         (car l-out)
                         (cons '* l-out)))
        ((equal? 0 (simplify (car l-in))) 0)
        ((equal? 1 (simplify (car l-in))) (loop (cdr l-in) l-out))
        (else (loop (cdr l-in) (append l-out (list (simplify (car l-in)))))))))
     (else (cons (car expr) (simplify (cdr expr))))))

(define-syntax flatten
  (syntax-rules()
    ((_ (expr ...))
     (begin
       (eval
        (let loop ((l-in '(expr ...)) (l-out '()))
          (if (not(null? l-in))
              (if (list? l-in)
                  (loop (car l-in) (loop (cdr l-in) l-out))
                  (cons l-in l-out))
              l-out)) (interaction-environment))))))


(define (whitespace? x)
  (and (char? x) (char-whitespace? x)))

(define (list-trim-right xs)
  (define call/cc call-with-current-continuation)
  (define *env* #f)
  (let loop ((l-in xs) (stack #f))
    (cond
      ((null? l-in) (if stack
                        (*env* '())
                        '()))
      ((and (whitespace? (car l-in)) stack) (cons (car l-in) (loop (cdr l-in) stack)))
      ((and (whitespace? (car l-in)) (not stack)) (call/cc
                                                   (lambda (cc)
                                                     (begin
                                                       (set! *env* cc)
                                                       (cons (car l-in) (loop (cdr l-in) #t))))))
      ((and (not (whitespace? (car l-in))) stack) (cons (car l-in) (loop (cdr l-in) #f)))
      ((and (not (whitespace? (car l-in))) (not stack)) (cons (car l-in) (loop (cdr l-in) stack))))))
    