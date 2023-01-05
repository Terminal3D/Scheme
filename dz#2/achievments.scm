;;; ------------------------------------------------
;;; my-flatten без append с хвостовой рекурсией
(define (my-flatten xs)
  (let loop ((c-list xs) (l-out '()))
    (if (not(null? c-list))
        (if (list? c-list)
            (loop (car c-list) (loop (cdr c-list) l-out))
            (cons c-list l-out))
        l-out)))       
;;; ------------------------------------------------

;;; ------------------------------------------------
;;; Композиция функций, используя my-fold-right
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
;;; ------------------------------------------------

;;; ------------------------------------------------
;;; list-trim-right не за линейное время, без reverse.
(define (is-whitespace? x)
  (and (char? x) (char-whitespace? x)))

(define (list-trim-right xs)
  (let loop ((in xs) (out '()) (temp '()) (stat #f))
    (cond
      ((null? in) out)
      ((is-whitespace? (car in)) (loop (cdr in) out (append (list (car in)) temp) #t))
      (else
       (if stat
           (loop in (append out temp) '() #f)
           (loop (cdr in) (append out (list (car in))) '() stat))))))
;;; ------------------------------------------------

;;; ------------------------------------------------
;;; list-trim-right за O(n) - обрезает за 2 прогона
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
;;; ------------------------------------------------

;;; ------------------------------------------------
;;; list-trim-right за O(n) - обрезает за один прогон
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
;;; ------------------------------------------------   