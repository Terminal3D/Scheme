(define (my-range a b d)
  (define (loop addout cura)
    (if (< cura b)
        (loop (append addout (cons cura '()))
              (+ cura d))
        addout))
  (loop '() a))

;;=================================

(define (my-flatten xs)
  (if (not(null? xs))
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))
      '()))

;;=================================

(define (my-filter pred? xs)
  (if (not(null? xs))
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))
      '()))

;;=================================

(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (if (= (length xs) 1)
          (car xs)
          '())
      (op (car xs) (my-fold-right op (cdr xs)))))

;;=================================

(define (my-fold-left op xs)
  (let loop ((rxs (reverse xs)))
    (if (<= (length rxs) 1)
        (car rxs)
        (op (loop (cdr rxs)) (car rxs)))))


;;=================================
(define (my-element? x xs)
  (if (not (null? xs))
      (if (equal? x (car xs))
          #t
          (my-element? x (cdr xs)))
      #f))
; O(n)

(define (list->set xs)
  (if (not(null? xs))
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))
      '()))
; O(n^2)
;;=================================

(define (set? xs)
  (if (not(null? xs))
      (if (my-element? (car xs) (cdr xs))
          #f
          (set? (cdr xs)))
      #t))
;O(n^2)

;;=================================

(define (union xs ys)
  (if (>= (length xs) (length ys))
      (if (not(null? xs))
          (if (my-element? (car xs) ys)
              (union (cdr xs) ys)
              (cons (car xs) (union (cdr xs) ys)))
          ys)
      (if (not(null? ys))
          (if (my-element? (car ys) xs)
              (union (cdr ys) xs)
              (cons (car ys) (union (cdr ys) xs)))
          xs)))
;O(n^2)
;;=================================

(define (intersection xs ys)
  (if (not(null? xs))
      (if (my-element? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))
      '()))
;O(n^2)
;;================================= 

(define (difference xs ys)
  (if (not(null? xs))
      (if (my-element? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))
      '()))
;O(n^2)
;;=================================

(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))
;O(n^2)
;;=================================

(define (set-eq? xs ys)
  (if (= (length(union xs ys)) (length(intersection xs ys)))
      #t
      #f))
;O(n^2)
;;=================================

(define (string-trim-left astr)
  (let loop ((xs (string->list astr)))
    (if (char-whitespace? (car xs))
      (loop (cdr xs))
      (list->string xs))))

;;=================================

(define (string-trim-right astr)
  (let loop ((xs (reverse (string->list astr))))
    (if (char-whitespace? (car xs))
      (loop (cdr xs))
      (list->string (reverse xs)))))

;;=================================

(define (string-trim astr)
  (string-trim-left (string-trim-right astr)))

;;=================================

(define (string-prefix? a b)
  (let loop ((prefix (string->list a)) (xs (string->list b)))
    (cond
      ((null? prefix) #t)
      ((equal? (car prefix) (car xs)) (loop (cdr prefix) (cdr xs)))
      (else #f))))

;;=================================

(define (string-suffix? a b)
  (string-prefix? (list->string (reverse (string->list a)))
                  (list->string (reverse (string->list b)))))

;;=================================

(define (string-infix? a b)
  (cond
    ((> (string-length a) (string-length b)) #f)
    ((string-prefix? a b) #t)
    (else (string-infix? a (substring b 1)))))

;;=================================  

(define (string-split str sep)
  (define strl (string->list str))
  (if (not(null? strl))
      (if (string-prefix? sep str)
          (string-split (substring str (string-length sep)) sep)
          (cons (make-string 1 (car strl)) (string-split (list->string(cdr strl)) sep)))
      '()))

;;=================================

(define (make-multi-vector sizes . vs)
  (cond ((equal? 0 (length vs)) (list 'mv sizes (make-vector (apply * sizes))))
        
        ((equal? 1 (length vs)) (list 'mv sizes (make-vector (apply * sizes) (car vs))))
        
        (else (write 'Error))))

(define (multi-vector? m)
  (and (equal? (car m) 'mv)))

(define (real-index sizes coords)
  (if (null? sizes)
      0
      (+ (* (car coords) (apply * (cdr sizes))) (real-index (cdr sizes) (cdr coords)))))

(define (multi-vector-set! m indices value)
  (vector-set! (caddr m) (real-index (cadr m) indices) value))

(define (multi-vector-ref m indices)
  (vector-ref (caddr m) (real-index (cadr m) indices)))

;;==================================
       
  

