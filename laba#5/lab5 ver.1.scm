
(define ie (interaction-environment))
(define math-signums `(+ - *))
(define logic-signums `(= < >))


(define (in-list? x xs)
  (and (not(null? xs))
       (or (equal? (car xs) x)
           (in-list? x (cdr xs)))))

(define (word-index word program index)
  (if (< index (vector-length program))
      (if (equal? (vector-ref program index) word)
          index
          (word-index word program (+ index 1)))
      #f))


(define (executor action stack)
  (eval (list action (cadr stack) (car stack)) ie))


(define (interpret program input-stack)
  (let interpreter ((index 0) (stack input-stack) (return-stack '()) (dictionary '()))
    (if (= (vector-length program) index)
        stack
        (let ((word (vector-ref program index)))
          (cond
            ((number? word) (interpreter (+ index 1) (cons word stack) return-stack dictionary))
            ((equal? '/ word) (interpreter (+ index 1)
                                           (cons (quotient (cadr stack) (car stack)) (cddr stack))
                                           return-stack dictionary))
            ((in-list? word math-signums) (interpreter (+ index 1) (cons (executor word stack) (cddr stack)) return-stack dictionary))
            ((equal? word 'neg) (interpreter (+ index 1) (cons (- (car stack)) (cdr stack)) return-stack dictionary))
            ((equal? word 'mod) (interpreter (+ index 1) (cons (remainder (cadr stack) (car stack)) (cddr stack)) return-stack dictionary))
            ((in-list? word logic-signums) (interpreter (+ index 1) (cons (if (executor word stack) -1 0) (cddr stack)) return-stack dictionary))
            ((equal? word 'not) (interpreter (+ index 1) (cons (if (= (car stack) -1) 0 -1) (cdr stack)) return-stack dictionary))
            ((equal? word 'and) (interpreter (+ index 1) (cons (if (and (= (car stack) -1) (= (cdr stack) -1)) -1 0) (cddr stack)) return-stack dictionary))
            ((equal? word 'or) (interpreter (+ index 1) (cons (if (or (= (car stack) -1) (= (cdr stack) -1)) -1 0) (cddr stack)) return-stack dictionary))
            ((equal? word 'drop) (interpreter (+ index 1) (cdr stack) return-stack dictionary))
            ((equal? word 'swap) (interpreter (+ index 1) (append (list (cadr stack) (car stack)) (cddr stack)) return-stack dictionary))
            ((equal? word 'dup) (interpreter (+ index 1) (cons (car stack) stack) return-stack dictionary))
            ((equal? word 'over) (interpreter (+ index 1) (cons (cadr stack) stack) return-stack dictionary))
            ((equal? word 'rot) (interpreter (+ index 1) (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) return-stack dictionary))
            ((equal? word 'depth) (interpreter (+ index 1) (cons (length stack) stack) return-stack dictionary))
            ((equal? word 'define) (interpreter (+ (word-index 'end program index) 1) stack return-stack (cons (list (vector-ref program (+ index 1)) (+ index 2)) dictionary)))
            ((in-list? word '(exit end)) (interpreter (car return-stack) stack (cdr return-stack) dictionary))
            ((equal? word 'if) (interpreter (if (zero? (car stack)) (+ (word-index 'endif program index) 1) (+ index 1)) (cdr stack) return-stack dictionary))
            ((equal? word 'endif) (interpreter (+ index 1) stack return-stack dictionary))
            (else (interpreter (cadr (assoc word dictionary)) stack (cons (+ index 1) return-stack) dictionary)))))))




(define s-tests
  (list
   (test (interpret #(   define abs
                          dup 0 <
                          if neg endif
                          end
                          9 abs
                          -9 abs      ) (quote ())) '(9 9))
   (test (interpret #(   define =0? dup 0 = end
                          define <0? dup 0 < end
                          define signum
                          =0? if exit endif
                          <0? if drop -1 exit endif
                          drop
                          1
                          end
                          0 signum
                          -5 signum
                          10 signum       ) (quote ())) '(1 -1 0))
   (test (interpret #(   define -- 1 - end
                          define =0? dup 0 = end
                          define =1? dup 1 = end
                          define factorial
                          =0? if drop 1 exit endif
                          =1? if drop 1 exit endif
                          dup --
                          factorial
                          *
                          end
                          0 factorial
                          1 factorial
                          2 factorial
                          3 factorial
                          4 factorial     ) (quote ())) '(24 6 2 1 1))
   (test (interpret #(   define =0? dup 0 = end
                          define =1? dup 1 = end
                          define -- 1 - end
                          define fib
                          =0? if drop 0 exit endif
                          =1? if drop 1 exit endif
                          -- dup
                          -- fib
                          swap fib
                          +
                          end
                          define make-fib
                          dup 0 < if drop exit endif
                          dup fib
                          swap --
                          make-fib
                          end
                          10 make-fib     ) (quote ())) '(0 1 1 2 3 5 8 13 21 34 55))
   (test (interpret #(   define =0? dup 0 = end
                          define gcd
                          =0? if drop exit endif
                          swap over mod
                          gcd
                          end
                          90 99 gcd
                          234 8100 gcd    ) '()) '(18 9))
   ))

(run-tests s-tests)
















