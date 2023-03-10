(define (trans xinp)
  (cond
    ((list? xinp) (cons '("list") (cons xinp '())))
    ((vector? xinp) (cons '("vector") (cons (vector->list xinp) '())))
    ((string? xinp) (cons '("string") (cons (string->list xinp) '())))
    (else (cons "undefined" (cons "Enter string/number/vector" '())))))

(define (rtrans xinp)
  (cond
    ((equal? (car xinp) '("list")) (cadr xinp))
    ((equal? (car xinp) '("vector")) (list->vector (cadr xinp)))
    ((equal? (car xinp) '("string")) (list->string (cadr xinp)))))