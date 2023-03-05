(define call/cc call-with-current-continuation)
(define ie (interaction-environment))
(define *env* #t)
(define *env1* #t)
(define *env2* #t)
(define *env3* #t)



(define (in-list? x xs)
  (and (not(null? xs))
       (or (equal? (car xs) x)
           (in-list? x (cdr xs)))))

(define (pos-in-list x xs)
  (let loop ((lst xs) (index 0))
    (if (not (null? lst))
        (if (equal? x (car lst))
            index
            (loop (cdr lst) (+ index 1)))
        #f)))


;;<fracs> ::= <frac> | <frac> <fracs>
;;<frac> ::= <signum> <number> "/" <number>
;;<signum> ::= "+" | "-"
;;<number> ::= <digit> | <digit> <number>
;;<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9


;;<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(define char-digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;;<number> ::= <digit> | <digit> <number>
(define (str-number? strin)
  (if (call/cc (lambda (cc)
                 (begin
                   (set! *env1* cc)
                   (map (lambda (x)
                          (if (in-list? x char-digits)
                              x
                              (*env1* #f))) (string->list strin)))))
      #t
      #f))

;;<signum> ::= "+" | "-"
(define char-sign '(#\- #\+))
(define (char-sign? char-in)
  (and (in-list? char-in char-sign)))

;;<frac> ::= <signum> <number> "/" <number>
(define (frac->lex str-in)
  (call/cc (lambda (cc)
             (begin
               (set! *env* cc)
               (define slash-pos (pos-in-list #\/ str-in))
               (or slash-pos
                   (*env* #f))
               (let ((c-list str-in) (index 0) (list-out '((sgn +) (nom 1) (denom 1))))
                 (cond ((equal? (car c-list) #\+) 
                        (begin
                          (set-car! (cdr (assoc 'sgn list-out)) '+)
                          (set! index (+ index 1))))
                       ((equal? (car c-list) #\-)
                        (begin
                          (set-car! (cdr (assoc 'sgn list-out)) '-)
                          (set! index (+ index 1))))
                       ((in-list? (car c-list) char-digits) (set-car! (cdr (assoc 'sgn list-out)) '+))
                       (else (*env* #f)))
                 (let nom-loop ((nom-out '()))
                   (if (< index slash-pos)
                       (if (in-list? (list-ref c-list index) char-digits)
                           (begin
                             (set! index (+ index 1))
                             (nom-loop (cons (list-ref c-list (- index 1)) nom-out)))
                           (*env* #f))
                       (begin
                         (set! index (+ slash-pos 1))
                         (set-car! (cdr (assoc 'nom list-out)) (reverse nom-out)))))
                 (let denom-loop ((denom-out '()))
                   (if (<= index (- (length str-in) 1))
                       (if (in-list? (list-ref c-list index) char-digits)
                           (begin
                             (set! index (+ index 1))
                             (denom-loop (cons (list-ref c-list (- index 1)) denom-out)))
                           (*env* #f))
                       (begin
                         (set-car! (cdr (assoc 'denom list-out)) (reverse denom-out)))))
      
                 list-out)))))

(define (check-frac str-input)
  (if (frac->lex (string->list str-input))
      #t
      #f))

(define (scan-frac str-in)
  (and (check-frac str-in)
       (let ((lexic (frac->lex (string->list str-in))))
         (eval (list '/ (list (cadr (assoc 'sgn lexic)) (string->number (list->string (cadr (assoc 'nom lexic))))) (string->number (list->string (cadr (assoc 'denom lexic))))) ie))))
                           

(define (string-prefix? a b)
  (if (<= (string-length a) (string-length b))
      (if (equal? (substring b 0 (string-length a)) a)
          #t
          #f)
      #f))

(define (string-split-whitespace str)
  (define strl (string->list str))
  (define lout '())
  (define clist '())
  (define index 0)
  (map (lambda (x)
         (if (char-whitespace? x)
             (begin
               (or (null? clist)
                   (set! lout (cons (list->string (reverse clist)) lout)))
               (set! clist '())
               (set! index (+ index 1)))
             (set! clist (cons x clist)))) strl)
  (if (null? clist)
      (reverse lout)
      (reverse (cons (list->string (reverse clist)) lout))))

(define (scan-many-fracs str-in)
  (let fracs->fracs-list ((fracs-in (string-split-whitespace str-in)) (fracs-out '()))
    (if (in-list? #f fracs-out)
        #f
        (cond
          ((null? fracs-in) (reverse fracs-out))
          (else (fracs->fracs-list (cdr fracs-in) (cons (scan-frac (car fracs-in)) fracs-out)))
          ))))
                       
              
        



;;=============================================

;;<Program>  ::= <Articles> <Body> .
;;<Articles> ::= <Article> <Articles> | .
;;<Article>  ::= define word <Body> end .
;;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define *break* #t)
(define *break1* #t)
(define *break2* #t)
(define *break3* #t)


(define (list-spliter xs x)
  (let loop ((c-list xs) (list-out '()))
    (if (not(null? c-list))
        (if (equal? (car c-list) x)
            (cons (reverse list-out) (list c-list))
            (loop (cdr c-list) (cons (car c-list) list-out)))
        #f)))

(define (if-endif-depth program)
  (let loop ((body program) (l-out '()) (depth 1))
    (if (or (not(null? body)) (zero? depth))
        (if (not(zero? depth))
            (cond ((equal? (car body) 'if) (loop (cdr body) (cons 'if l-out) (+ depth 1)))
                  ((equal? (car body) 'endif) (loop (cdr body) (cons 'endif l-out) (- depth 1)))
                  (else (loop (cdr body) (cons (car body) l-out) depth)))
            body)
        #f)))

(define (in-list-count xs x)
  (let loop ((body xs) (counter 0))
    (cond ((null? body) counter)
          ((equal? (car body) x) (loop (cdr body) (+ counter 1)))
          (else (loop (cdr body) counter)))))
      

(define (define-end program)
  (define define-count (in-list-count program 'define))
  (let loop ((c-list program) (end-count 0) (l-out '()))
    (cond ((not(equal? define-count (in-list-count program 'end))) #f)
          ((null? c-list) (cons (reverse l-out) (list '())))
          ((= define-count end-count) (cons (reverse l-out) (list c-list)))
          ((equal? (car c-list) 'end) (loop (cdr c-list) (+ end-count 1) (cons 'end l-out)))
          (else (loop (cdr c-list) end-count (cons (car c-list) l-out))))))

(define (define-assoc xs)
  (let loop ((c-list xs) (l-out '()) (stack '()))
    (cond  ((null? c-list) (reverse l-out))
           ((equal? (car c-list) 'define) (loop (cdr c-list) l-out (cons 'define stack)))
           ((equal? (car c-list) 'end) (loop (cdr c-list) (cons (reverse (cons 'end stack)) l-out) '()))
           (else (loop (cdr c-list) l-out (cons (car c-list) stack))))))


(define (parse-body program)
  (call/cc (lambda (cc)
             (begin
               (set! *break* cc)
               (let loop ((body program) (lexic-out '()) (if-status #f))
                 (if (not(null? body))
                     (let ((head (car body)))
                       (cond ((equal? head 'if)
                              (if (if-endif-depth (cdr body))
                                  (loop (if-endif-depth (cdr body)) (append lexic-out (list (list 'if (loop (cdr body) '() #t)))) if-status) 
                                  (*break* #f)))
                             ((equal? head 'endif)
                              (if if-status
                                  lexic-out
                                  (*break* #f)))
                             (else (loop (cdr body) (append lexic-out (list head)) if-status))))
                     lexic-out))))))

(define (parse-article program)
  (define article-split (list-spliter program 'end))
  (call/cc (lambda (cc)
             (begin
               (set! *break1* cc)
               (let ((article (cdar article-split)) (lexic-out '()))
                 (if (symbol? (car article))
                     (set! lexic-out (car article))
                     (*break1* #f))
                 (if (parse-body (cdr article))
                     (cons lexic-out (list (parse-body (cdr article))))
                     (*break1* #f)))))))



(define (parse-articles program)
  (call/cc (lambda (cc)
             (begin
               (set! *break2* cc)
               (let loop ((c-list (define-assoc program)) (l-out '()))
                 (cond ((null? c-list) (reverse l-out))
                       (else (if (parse-article (car c-list))
                                 (loop (cdr c-list) (cons (parse-article (car c-list)) l-out))
                                 (*break2* #f)))))))))
               

(define (parse program)
  (call/cc (lambda (cc)
             (begin
               (set! *break3* cc)
               (if (define-end (vector->list program))
                   #t
                   (*break3* #f))
               (define articles (car (define-end (vector->list program))))
               (define body (cadr (define-end (vector->list program))))
               (if (parse-articles articles)
                   (if (parse-body body)
                       (list (parse-articles articles) (parse-body body))
                       #f)
                   #f)))))




     
    
    
    



