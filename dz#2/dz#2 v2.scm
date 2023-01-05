
(define (my-range a b d)
  (let loop ((cur a))
    (cond
      ((> cur b) '())
      (else (cons cur (loop (+ cur d)))))))

;; my-flatten с хвостовой рекурсией без append
(define (my-flatten xs)
  (let loop ((in xs) (out '()))
    (cond
      ((null? in) out)
      ((pair? in) (loop (car in) (loop (cdr in) out)))
      (else (cons in out)))))

(define (my-element? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (cond
    ((null? xs) '())
    ((pred? (car xs)) (cons (car xs) (my-filter pred? (cdr xs))))
    (else (my-filter pred? (cdr xs)))))

(define (my-fold-left op xs)
  (cond
    ((= (length xs) 1) (car xs))
    (else (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs))))))
  

(define (my-fold-right op xs)
  (cond
    ((= (length xs) 1) (car xs))
    (else (op (car xs) (my-fold-right op (cdr xs))))))

;;------------------------------------------

(define (in? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (in? x (cdr xs)))))
;;O(n)

(define (list->set xs)
  (cond
    ((null? xs) '())
    ((in? (car xs) (cdr xs)) (list->set (cdr xs)))
    (else (cons (car xs) (list->set (cdr xs))))))
;;O(n^2)

(define (set? xs)
  (cond
    ((null? xs) #t)
    ((in? (car xs) (cdr xs)) #f)
    (else (set? (cdr xs)))))
;;O(n^2)

(define (union xs ys)
  (append ys
          (let loop ((in xs))
            (cond
              ((null? in) '())
              ((in? (car in) ys) (loop (cdr in)))
              (else (cons (car in) (loop (cdr in))))))))
;;O(n^2)


(define (intersection xs ys)
  (let loop ((in xs))
    (cond
      ((null? in) '())
      ((in? (car in) ys) (cons (car in) (loop (cdr in))))
      (else (loop (cdr in))))))

;;O(n^2)


(define (difference xs ys)
  (let loop ((in xs))
    (cond
      ((null? in) '())
      ((in? (car in) ys) (loop (cdr in)))
      (else (cons (car in) (loop (cdr in)))))))
;;O(n^2)

(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))
;;O(n^2)


(define (set-eq? xs ys)
  (and (= (length xs) (length ys)) (= (length xs) (length (intersection xs ys)))))
;;O(n^2)


;;------------------------------------------

(define (is-whitespace? x)
  (and (char? x) (char-whitespace? x)))

(define (string-trim-left s)
  (list->string
   (let loop ((in (string->list s)))
     (cond
       ((null? in) '())
       ((is-whitespace? (car in)) (loop (cdr in)))
       (else in)))))

(define (string-trim-right s)
  (list->string (reverse (string->list (string-trim-left (list->string (reverse (string->list s))))))))

(define (string-trim s)
  (string-trim-left (string-trim-right s)))

(define (string-prefix? a b)
  (if (> (string-length a) (string-length b))
      #f
      (let loop ((pref (string->list a)) (str (string->list b)))
        (cond
          ((null? pref) #t)
          ((equal? (car pref) (car str)) (loop (cdr pref) (cdr str)))
          (else #f)))))

(define (string-suffix? a b)
  (if (> (string-length a) (string-length b))
      #f
      (let loop ((pref (reverse (string->list a))) (str (reverse (string->list b))))
        (cond
          ((null? pref) #t)
          ((equal? (car pref) (car str)) (loop (cdr pref) (cdr str)))
          (else #f)))))


(define (string-infix? a b)
  (if (> (string-length a) (string-length b))
      #f
      (let loop ((pref (string->list a)) (str (string->list b)))
        (cond
          ((null? pref) #t)
          ((null? str) #f)
          ((equal? (car pref) (car str)) (loop (cdr pref) (cdr str)))
          (else (loop (string->list a) (cdr str)))))))

(define (string-split str sep)
  (let loop ((in (string->list str)))
    (cond
      ((null? in) '())
      ((string-prefix? sep (list->string in)) (loop
                                               (let inner-loop ((l-in in) (index (string-length sep)))
                                                 (if (= index 0)
                                                     l-in
                                                     (inner-loop (cdr l-in) (- index 1))))))
      (else (cons (make-string 1 (car in)) (loop (cdr in)))))))



;;------------------------------------------

(define (make-multi-vector sizes . fill)
  (if (equal? 0 (length fill))
      (list 'm sizes (make-vector (apply * sizes)))
      (list 'm sizes (make-vector (apply * sizes) (car fill)))))

(define (multi-vector? m)
  (and (equal? (car m) 'm)))

(define (get-index sizes indices)
  (if (null? sizes)
      0
      (+ (* (car indices) (apply * (cdr sizes))) (get-index (cdr sizes) (cdr indices)))))

(define (multi-vector-set! m indices value)
  (vector-set! (caddr m) (get-index (cadr m) indices) value))

(define (multi-vector-ref m indices)
  (vector-ref (caddr m) (get-index (cadr m) indices)))

;;------------------------------------------

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (my-fold-left (lambda (x f)
                    (f x))
                  (cons x (reverse xs)))))


;;------------------------------------------

