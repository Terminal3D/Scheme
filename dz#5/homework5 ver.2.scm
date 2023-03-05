(define feature-if-else #t)
(define feature-nested-if #t)
(define feature-while-loop #t)
(define feature-repeat-loop #t)
(define feature-for-loop #t)
(define feature-break-continue #t)
(define feature-switch-case #t)
(define feature-hi-level #t)
(define feature-tail-call #t)
(define feature-global #t)

(define ie (interaction-environment))
(define (my-element? x xs)
  (if (null? xs)
      #f
      (if (equal? x (car xs))
          #t
          (my-element? x (cdr xs)))))

(define (interpret program stack)
  (define program-length (vector-length program))
  (define (depth-finder ind start end)
    (let loop ((eg 1) (index ind))
      
      (if (>= index program-length)
          (if (zero? eg)
              index
              #f)
          (let ((top (vector-ref program index)))
            (cond
              ((zero? eg) index)
              ((equal? top start) (loop (+ eg 1) (+ index 1)))
              ((equal? top end) (loop (- eg 1) (+ index 1)))
              (else (loop eg (+ index 1))))))))

  (define (case-finder ind case)
    (let loop ((index ind) (top (vector-ref program ind)))
      (if (>= index program-length)
          #f
          (cond
            ((and (equal? top 'case) (equal? (vector-ref program (+ index 1)) case))
             (+ index 2))
            ((equal? top 'endswitch) (+ index 1))
            (else (loop (+ index 1) (vector-ref program (+ index 1))))))))
      
    
        
  (let i-pret ((ind 0) (st stack) (r-st '()) (dict '()))
    (if (>= ind program-length)
        st
        (let ((top (vector-ref program ind)))
          (cond
            ((number? top) (i-pret (+ ind 1) (cons top st) r-st dict))
            ((my-element? top '(+ - *)) (i-pret (+ ind 1)
                                                (cons (eval (list top (cadr st) (car st)) ie) (cddr st))
                                                r-st dict))
            ((my-element? top '(> < =)) (i-pret (+ ind 1)
                                                (cons (if (eval (list top (cadr st) (car st)) ie)
                                                          -1
                                                          0)
                                                      (cddr st)) r-st dict))
            ((equal? top '/) (i-pret (+ ind 1) (cons (quotient (cadr st) (car st)) (cddr st)) r-st dict))
            ((equal? top 'mod) (i-pret (+ ind 1) (cons (remainder (cadr st) (car st)) (cddr st)) r-st dict))
            ((equal? top 'neg) (i-pret (+ ind 1) (cons (- (car st)) (cdr st)) r-st dict))
            ((equal? top 'and) (i-pret (+ ind 1) (cons (if (and (not (zero? (cadr st)))
                                                                (not (zero? (car st))))
                                                           -1
                                                           0) (cddr st)) r-st dict))
            ((equal? top 'or) (i-pret (+ ind 1) (cons (if (or (not (zero? (cadr st)))
                                                              (not (zero? (car st))))
                                                          -1
                                                          0) (cddr st)) r-st dict))
            ((equal? top 'not) (i-pret (+ ind 1) (cons (if (zero? (car st))
                                                           -1
                                                           0) (cdr st)) r-st dict))
            ((equal? top 'drop) (i-pret (+ ind 1) (cdr st) r-st dict))
            
            ((equal? top 'swap) (i-pret (+ ind 1) (append (list (cadr st) (car st)) (cddr st))
                                        r-st dict))
            ((equal? top 'dup) (i-pret (+ ind 1) (cons (car st) st) r-st dict))
            ((equal? top 'over) (i-pret (+ ind 1) (cons (cadr st) st) r-st dict))
            ((equal? top 'rot) (i-pret (+ ind 1)
                                       (append (list (caddr st) (cadr st) (car st)) (cdddr st)) r-st dict))
            ((equal? top 'depth) (i-pret (+ ind 1) (cons (length st) st) r-st dict))
            ((equal? top 'define) (i-pret (depth-finder (+ ind 1) 'define 'end) st r-st
                                          (cons (list (vector-ref program (+ ind 1)) (+ ind 2)) dict)))
            ((equal? top 'end) (i-pret (car r-st) st (cdr r-st) dict))
            ((equal? top 'exit) (i-pret (car r-st) st (cdr r-st) dict))
            ((equal? top 'endif) (i-pret (+ ind 1) st r-st dict))
            ((equal? top 'else) (i-pret (depth-finder (+ ind 1) 'if 'endif) st r-st dict))
            ((equal? top 'if) (if (not(zero? (car st)))
                                  (i-pret (+ ind 1) (cdr st) r-st dict)
                                  (if (and (depth-finder (+ ind 1) 'if 'else)
                                           (> (depth-finder (+ ind 1) 'if 'endif)
                                              (depth-finder (+ ind 1) 'if 'else)))
                                      (i-pret (depth-finder (+ ind 1) 'if 'else) (cdr st) r-st dict)
                                      (i-pret (depth-finder (+ ind 1) 'if 'endif) (cdr st) r-st dict))))
            ((equal? top 'while) (if (zero? (car st))
                                     (i-pret (depth-finder (+ ind 1) 'while 'wend) (cdr st) r-st dict)
                                     (i-pret (+ ind 1) (cdr st) (cons ind r-st) dict)))
            ((equal? top 'wend) (i-pret (car r-st) st (cdr r-st) dict))
            ((equal? top 'repeat) (i-pret (+ ind 1) st (cons ind r-st) dict))
            ((equal? top 'until) (i-pret (if (zero? (car st))
                                             (car r-st)
                                             (+ ind 1)) (cdr st) (cdr r-st) dict))
            ((equal? top 'for) (i-pret (+ ind 1) (cddr st)
                                       (cons (list (cadr st) (car st) (+ ind 1)) r-st) dict))
            ((equal? top 'i) (i-pret (+ ind 1) (cons (caar r-st) st) r-st dict))
            ((equal? top 'next) (if (<= (+ (caar r-st) 1) (cadar r-st))
                                    (i-pret (caddar r-st) st
                                            (cons (list (+ (caar r-st) 1) (cadar r-st) (caddar r-st))
                                                  (cdr r-st)) dict)
                                    (i-pret (+ ind 1) st (cdr r-st) dict)))
            ((equal? top 'break) (i-pret
                                  (cond
                                    ((list? (car r-st)) (depth-finder ind 'for 'next))
                                    ((equal? 'repeat (vector-ref program (car r-st)))
                                     (depth-finder ind 'repeat 'until))
                                    (else (depth-finder ind 'while 'wend))) st (cdr r-st) dict))
            ((equal? top 'continue) (i-pret
                                     (cond
                                       ((list? (car r-st)) (- (depth-finder ind 'for 'next) 1))
                                       ((equal? 'repeat (vector-ref program (car r-st)))
                                        (- (depth-finder ind 'repeat 'until) 1))
                                       (else (- (depth-finder ind 'while 'wend) 1))) st r-st dict))
            ((equal? top 'switch) (i-pret (case-finder (+ ind 1) (car st)) (cdr st) r-st dict))
            ((equal? top 'exitcase) (i-pret (depth-finder ind '((abracadabra)) 'endswitch) st r-st dict))
            ((equal? top 'case) (i-pret (+ ind 2) st r-st dict))
            ((equal? top 'endswitch) (i-pret (+ ind 1) st r-st dict))
            ((equal? top '&) (i-pret (+ ind 2) (cons (cadr (assoc (vector-ref program (+ ind 1)) dict)) st)
                                     r-st dict))
            ((equal? top 'lam) (i-pret (depth-finder (+ ind 1) 'lam 'endlam) (cons (+ ind 1) st) r-st dict))
            ((equal? top 'endlam) (i-pret (car r-st) st (cdr r-st) dict))
            ((equal? top 'apply) (i-pret (car st) (cdr st) (cons (+ ind 1) r-st) dict))
            ((equal? top 'tail) (i-pret (cadr (assoc (vector-ref program (+ ind 1)) dict)) st r-st dict))
            ((equal? top 'defvar) (i-pret (+ ind 3) st r-st (cons
                                                             (list
                                                              (vector-ref program (+ ind 1))
                                                              (vector-ref program (+ ind 2)) 'abracadbra)
                                                             dict)))
            ((equal? top 'set)
             (let ()
               (set-car! (cdr (assoc (vector-ref program (+ ind 1)) dict)) (car st))
               (i-pret (+ ind 2) (cdr st) r-st
                       dict)))
            
            
            

            (else (let ((var (assoc top dict)))
                    (if (= (length var) 2)
                        (i-pret (cadr var) st (cons (+ ind 1) r-st) dict)
                        (i-pret (+ ind 1) (cons (cadr var) st) r-st dict))))
             
            
            )))))


