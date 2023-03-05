;<expr> ::= <token> | <token> <expr>
;<token> ::= <symbol> | <number> | <open_paren> | <close_paren> | <operation>
;<symbol> ::= <letter> | <symbol> <letter_or_digit>
;<number> ::= <integer> | <float> | <scientific_notation>
;<integer> ::= <digit> | <digit> <integer>
;<float> ::= <integer> "." <integer>
;<scientific_notation> ::= <integer_or_float> <e> <integer>
;<integer_or_float> ::= <integer> | <float>
;<open_paren> ::= "("
;<close_paren> ::= ")"
;<operation> ::= + | - | * | / | ^
;<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
;<digit> ::= "0" | "1" | ... | "9"
;<letter_or_digit> ::= <letter> | <digit>
;<e> ::= "e" | "e" "-"

(define char-letter? char-alphabetic?)
(define char-digit? char-numeric?)

;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Запрос первых двух символов
(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdar stream))
          (list (caar stream))
          (list (caar stream) (cadar stream)))))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

(define (in-list? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (in-list? x (cdr xs)))))

(define (in-stream? x stream)
  (in-list? x (car stream)))

(define call/cc call-with-current-continuation)

(define (tokenize token)
  (define break #f)
  (define operations '(#\+ #\- #\* #\/ #\^))
  (define (scan-expr stream)
    (cond
      ((not(peek stream)) '())
      ((char-whitespace? (peek stream)) (begin
                                          (next stream)
                                          (scan-expr stream)))
      (else (cons (scan-token stream) (scan-expr stream)))))

  (define (scan-token stream)

    (cond
      ((char-letter? (peek stream)) (string->symbol (list->string (scan-symbol stream))))
      ((char-digit? (peek stream)) (string->number (list->string (scan-number stream))))
      ((in-list? (peek stream) operations) (string->symbol (make-string 1 (next stream))))
      ((equal? (peek stream) #\() (begin
                                    (next stream)
                                    "("))
      ((equal? (peek stream) #\)) (begin
                                    (next stream)
                                    ")"))
      (else (break #f))))

  (define (scan-symbol stream)
    (cond
      ((not(peek stream)) '())
      ((char-whitespace? (peek stream)) '())
      ((or (in-list? (peek stream) operations) (in-list? (peek stream) '(#\( #\)))) '()) 
      ((char-letter? (peek stream)) (cons (char-downcase (next stream)) (scan-symbol stream)))
      ((char-digit? (peek stream)) (cons (next stream) (scan-symbol stream)))
      (else (break #f))))

  (define (scan-number stream)
    (define temp #f)
    (scan-number1 (make-stream
                   (let loop ((prev #f))
                     (set! temp (peek stream))
                     (cond
                       ((not(peek stream)) '())
                       ((in-list? (peek stream) '(#\( #\))) '())
                       ((in-list? (peek stream) operations)
                        (if (and (equal? (peek stream) #\-) (equal? prev #\e))
                            (cons (next stream) (loop temp))
                            '()))
                       ((char-whitespace? (peek stream)) '())
                       (else (cons (next stream) (loop temp))))))))

  (define (scan-number1 number)
    (cond
      ((in-stream? #\e number) (scan-scient_notation number))
      ((in-stream? #\. number) (scan-float number))
      (else (scan-integer number))))

  (define (scan-scient_notation number)
    (let loop ((stream number) (left '()))
      (cond
        ((equal? #\e (peek stream))
         (append (scan-integer_or_float (make-stream (reverse left))) (scan-e stream) (scan-integer stream)))
        (else (loop stream (cons (next stream) left))))))

  (define (scan-integer_or_float number)
    (if (in-stream? #\. number)
        (scan-float number)
        (scan-integer number)))

  (define (scan-float number)
    (let loop ((stream number) (left '()))
      (cond
        ((and (equal? (peek stream) #\.) (peek2 stream) (not(null? left)))
         (append (scan-integer (make-stream (reverse left))) (list (next stream))
                 (scan-integer stream)))
        (else (loop stream (cons (next stream) left))))))

  (define (scan-integer number)
    (cond
      ((not(peek number)) '())
      ((char-whitespace? (peek number)) '())
      ((char-digit? (peek number)) (cons (next number) (scan-integer number)))
      (else (break #f))))

  (define (scan-e ex)
    (if (equal? #\- (cadr (peek2 ex)))
        (list (next ex) (next ex))
        (list (next ex))))

  (call/cc (lambda (halt) (set! break halt)
             (define stream (make-stream (string->list token)))
             (scan-expr stream))))



;-----------

;Expr    ::= Term Expr' .
;Expr'   ::= AddOp Term Expr' | .
;Term    ::= Factor Term' .
;Term'   ::= MulOp Factor Term' | .
;Factor  ::= Power Factor' .
;Factor' ::= PowOp Power Factor' | .
;Power   ::= value | "(" Expr ")" | unaryMinus Power .



(define (parse token)
  
  (define break #f)
  (define addop '(+ -))
  (define mulop '(* /))
  
  (define (scan-expr stream)
    (let loop ((out (scan-term stream)))
      (cond
        ((in-list? (peek stream) addop)
         (loop (list out (next stream) (scan-term stream))))
        (else out))))

  (define (scan-term stream)

    (let loop ((out (scan-factor stream)))
      (cond
        ((in-list? (peek stream) mulop)
         (loop (list out (next stream) (scan-factor stream))))
        ((or (in-list? (peek stream) addop) (not (peek stream))) out)
        (else (break #f)))))

  (define (scan-factor stream)
         (let ((item (scan-power stream)))
           (cond
             ((equal? (peek stream) '^) (list item (next stream) (scan-factor stream)))
             (else item))))

  (define (scan-power stream)
    (cond
      
      ((number? (peek stream)) (next stream))
      ((equal? (peek stream) "(") (begin
                                    (next stream)
                                    (scan-expr (make-stream (get-expr stream 1)))))
      ((equal? (peek stream) ")") (break #f))
      ((equal? (peek stream) '-) (list (next stream) (scan-power stream)))
      ((symbol? (peek stream)) (next stream))
      (else (break #f))))

  (define (get-expr stream count)
    (cond
      ((not(peek stream)) (break #f))
      ((and (equal? (peek stream) ")") (= 1 count)) (begin
                                                      (next stream)
                                                      '()))
      ((equal? (peek stream) ")") (cons (next stream) (get-expr stream (- count 1))))
      ((equal? (peek stream) "(") (cons (next stream) (get-expr stream (+ count 1))))
      (else (cons (next stream) (get-expr stream count)))))

  (call/cc (lambda (halt) (set! break halt)
             (begin
               (define stream (make-stream token))
               (scan-expr stream)))))



;-----------

                                   
(define (tree->scheme xs)
  (cond
    ((not (pair? xs)) xs)
    ((= (length xs) 3) (if (equal? (cadr xs) '^)
                           (list 'expt (tree->scheme (car xs)) (tree->scheme (caddr xs)))
                           (list (cadr xs) (tree->scheme (car xs)) (tree->scheme (caddr xs)))))
    ((= (length xs) 2) (list '- (tree->scheme (cadr xs))))
    ((= (length xs) 1) (tree->scheme (car xs)))))
                         
    
      
    
      

   
      
          