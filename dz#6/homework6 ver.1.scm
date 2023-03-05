;; BNF for tokenize

;<токен> ::= <объект> <токен> | e
;<объект> ::= <скобки> | <операция> | <переменная> | <число>
;<переменная> ::= ПЕРЕМЕННАЯ 
;<число> ::= ЧИСЛО
;<операция> ::= + | - | * | / | ^
;<скобки> ::= ')' | '('

(define call/cc call-with-current-continuation)

(define (char->string x)
  (make-string 1 x))

(define (char->symbol x)
  (string->symbol (char->string x)))

(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdar stream))
          (list (caar stream))
          (list (caar stream) (cadar stream)))))

(define (peek-peek stream)
  (if (< (length (car stream)) 2)
      (cadr stream)
      (cadar stream)))

(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

(define (pop stream)
  (if (not (null? (car stream)))
      (set-car! stream (cdr (car stream))))
  stream)

(define operations '(#\+ #\- #\* #\/ #\^))
(define paranthesis '(#\( #\)))

;----------------------
 
(define (tokenize str)
  (define stream (make-stream (string->list str)))
  (define error #f)

  (define (tokenize-object stream)
    (cond
      ((member (peek stream) paranthesis) (tokenize-paranthesis stream))
      ((member (peek stream) operations) (tokenize-operations stream))
      ((char-alphabetic? (peek stream)) (tokenize-variable stream '()))
      ((char-numeric? (peek stream)) (tokenize-number stream '()))
      (else (error #f))))


  (define (tokenize-paranthesis stream)
    (char->string (next stream)))

  (define (tokenize-operations stream)
    (char->symbol (next stream)))

  (define (tokenize-variable stream out)
    (cond
      ((or (not (peek stream)) (not (char-alphabetic? (peek stream))))
       (string->symbol (list->string (reverse out))))
      (else (tokenize-variable stream (cons (char-downcase (next stream)) out)))))

  (define (tokenize-number stream out)
    (if (equal? (peek stream) #f)
        (string->number (list->string (reverse out)))
        (cond
          ((equal? (peek stream) #\.) (tokenize-number stream (cons (next stream) out)))
          ((equal? (peek stream) #\e)
           (cond
             ((equal? #f (peek-peek stream)) (error #f))
             (else (tokenize-number stream (cons (next stream) out)))))
          ((equal? #\- (peek stream))
           (cond
             ((equal? #\e (car out))
              (tokenize-number stream (cons (next stream) out)))
             (else (string->number (list->string (reverse out))))))
          ((char-numeric? (peek stream)) (tokenize-number stream (cons (next stream) out)))
          (else (if (or (member (peek stream) paranthesis)
                        (member (peek stream) operations)
                        (char-whitespace? (peek stream)))
                    (string->number (list->string (reverse out)))
                    (error #f)))
            )))

  (call/cc
   (lambda (cc) (set! error cc)
     (begin
       (let loop ((token stream))
         (cond
           ((not(peek token)) '())
           ((char-whitespace? (peek token)) (loop (pop token)))
           (else (cons (tokenize-object token) (loop token)))))))
   
   ))


;-------------------------------------


;Expr    ::= Term Expr' .
;Expr'   ::= AddOp Term Expr' | .
;Term    ::= Factor Term' .
;Term'   ::= MulOp Factor Term' | .
;Factor  ::= Power Factor' .
;Factor' ::= PowOp Power Factor' | .
;Power   ::= value | "(" Expr ")" | unaryMinus Power .


(define (parse token)
  (define error #f)

  (define (value? x)
    (or (number? x) (variable? x)))

  (define (variable? x)
    (if (or (equal? x #f) (equal? x ")") (equal? x "("))
        (error #f)
        (char-alphabetic? (string-ref (symbol->string x) 0))))
  
  (define (list-remover xs)
    (cond
      ((null? xs) (error #f))
      ((= (length xs) 1) (car xs))
      (else xs)))
  
  (define (expr stream)
    (let loop ((t (term stream)))
      (if (or (equal? '+ (peek stream)) (equal? '- (peek stream)))
          (loop (list t (next stream) (term stream)))
          t)))

  (define (term stream)
    (let loop ((f (list-remover (factor stream))))
      (if (or (equal? '* (peek stream)) (equal? '/ (peek stream)))
          (loop (list f (next stream) (list-remover (factor stream))))
          f)))


  (define (factor stream)
    (append (power stream) (factor1 stream)))

  (define (factor1 stream)
    (cond
      ((equal? '^ (peek stream))
       (list (next stream)
             (let ((t (append (power stream) (factor1 stream))))
               (cond
                 ((null? t) (error #f))
                 ((= (length t) 1) (car t))
                 (else t)))))
      (else '())))

  (define (power stream)
    (trace-ex stream)
    (cond
      ((equal? (peek stream) "(")
       (list (expr (make-stream
                    (let loop ((token (pop stream)) (count 1))
                      (cond
                        ((zero? count) '())
                        ((equal? (peek token) #f) (error #f))
                        ((equal? (peek token) "(")
                         (cons (next token) (loop token (+ count 1))))
                        ((equal? (peek token) ")")
                         (cons (next token) (loop token (- count 1))))
                        (else (cons (next token) (loop token count)))))))))
      ((equal? (peek stream) '-) (append (list (next stream)) (power stream)))
      ((value? (peek stream)) (list (next stream)))
      (else (error #f))))
  
  (define stream (make-stream token))
  (call/cc (lambda (cc) (set! error cc)
             (define res (expr stream))
             (and (not (peek stream))
                  res))))

;-----------------------------------------------


(define (tree->scheme tree)
  (cond
    ((not (pair? tree)) tree)
    ((= (length tree) 3) (if (equal? (cadr tree) '^)
                             (list 'expt (tree->scheme (car tree)) (tree->scheme (caddr tree)))
                             (list (cadr tree) (tree->scheme (car tree)) (tree->scheme (caddr tree)))))
    ((= (length tree) 2) (list '- (tree->scheme (cadr tree))))))