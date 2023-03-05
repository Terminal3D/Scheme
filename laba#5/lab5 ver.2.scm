(define (interpret program stack)
  (define ie (interaction-environment))
  (define (index-finder word index)
    (if (not (>= index (vector-length program)))
        (if (equal? (vector-ref program index) word)
            index
            (index-finder word (+ index 1)))
        #f))
    
  (define (calc op stack)
    (eval `(,op ,(cadr stack) ,(car stack)) ie))

        
  (let main ((index 0) (stack stack) (dict '()) (r-stack '()))
    (if (>= index (vector-length program))
        stack
        (let ((word (vector-ref program index)))
          (trace-ex stack)
          (trace-ex word)
          (cond
            ((number? word) (main (+ index 1) (cons word stack) dict r-stack))
            ((equal? word '+) (main (+ index 1) (cons (calc word stack)
                                                      (cddr stack)) dict r-stack))
            ((equal? word '*) (main (+ index 1) (cons (calc word stack)
                                                      (cddr stack)) dict r-stack))
            ((equal? word '-) (main (+ index 1) (cons (calc word stack)
                                                      (cddr stack)) dict r-stack))
            ((equal? word '/) (main (+ index 1) (cons (calc 'quotient stack)
                                                      (cddr stack)) dict r-stack))
            ((equal? word 'mod) (main (+ index 1) (cons (calc 'remainder stack)
                                                        (cddr stack)) dict r-stack))
            ((equal? word 'neg) (main (+ index 1) (cons (- (car stack))
                                                        (cdr stack)) dict r-stack))
            ((equal? word '=) (main (+ index 1) (cons (if (= (cadr stack) (car stack))
                                                          -1
                                                          0) (cddr stack)) dict r-stack))
            ((equal? word '>) (main (+ index 1) (cons (if (> (cadr stack) (car stack))
                                                          -1
                                                          0) (cddr stack)) dict r-stack))
            ((equal? word '<) (main (+ index 1) (cons (if (< (cadr stack) (car stack))
                                                          -1
                                                          0) (cddr stack)) dict r-stack))
            ((equal? word 'not) (main (+ index 1) (cons (if (= (car stack) 0)
                                                            -1
                                                            0) (cdr stack)) dict r-stack))
            ((equal? word 'and) (main (+ index 1) (cons (if (and (not(= (cadr stack) 0))
                                                                 (not(= (car stack) 0)))
                                                            -1
                                                            0) (cddr stack)) dict r-stack))
            ((equal? word 'or) (main (+ index 1) (cons (if (or (not(= (cadr stack) 0))
                                                               (not(= (car stack) 0)))
                                                           -1
                                                           0) (cddr stack)) dict r-stack))
            ((equal? word 'drop) (main (+ index 1) (cdr stack) dict r-stack))
            ((equal? word 'swap) (main (+ index 1) (append `(,(cadr stack) ,(car stack))
                                                           (cddr stack)) dict r-stack))
            ((equal? word 'dup) (main (+ index 1) (cons (car stack) stack) dict r-stack))
            ((equal? word 'over) (main (+ index 1) (cons (cadr stack) stack) dict r-stack))
            ((equal? word 'rot) (main (+ index 1) (append
                                                   `(,(caddr stack) ,(cadr stack) ,(car stack))
                                                   (cdddr stack)) dict r-stack))
            ((equal? word 'depth) (main (+ index 1) (cons (length stack) stack) dict r-stack))
            ((equal? word 'define) (main (+ (index-finder 'end index) 1) stack
                                         (cons `(,(vector-ref program (+ index 1))
                                                 ,(+ index 2)) dict) r-stack))
            ((equal? word 'end) (main (car r-stack) stack dict (cdr r-stack)))
            ((equal? word 'exit) (main (car r-stack) stack dict (cdr r-stack)))
            ((equal? word 'if) (main (if (= 0 (car stack))
                                         (index-finder 'endif (+ index 1))
                                         (+ index 1)) (cdr stack) dict r-stack))
            ((equal? word 'else) (main (+ (index-finder 'endif index) 1) stack dict r-stack))
            ((equal? word 'endif) (main (+ index 1) stack dict r-stack))
        (else 
                  (main (cadr (assoc word dict)) stack dict (cons (+ index 1) r-stack))))))))
                  




