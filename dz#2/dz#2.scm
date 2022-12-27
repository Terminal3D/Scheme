(load "trace.scm")

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

(define (list->set xs)
  (if (not(null? xs))
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))
      '()))

;;=================================

(define (set? xs)
  (if (not(null? xs))
      (if (my-element? (car xs) (cdr xs))
          #f
          (set? (cdr xs)))
      #t))

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

;;=================================

(define (intersection xs ys)
  (if (not(null? xs))
      (if (my-element? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))
      '()))

;;================================= 

(define (difference xs ys)
  (if (not(null? xs))
      (if (my-element? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))
      '()))

;;=================================

(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))

;;=================================

(define (set-eq? xs ys)
  (if (= (length(union xs ys)) (length(intersection xs ys)))
      #t
      #f))

;;=================================

(define (string-trim-left astr)
  (if (char-whitespace? (string-ref astr 0)) 
      (string-trim-left (substring astr 1))
      astr))

;;=================================

(define (string-trim-right astr)
  (if (char-whitespace? (string-ref astr (- (string-length astr) 1)))
      (string-trim-right (substring astr 0 (- (string-length astr) 1)))
      astr))

;;=================================

(define (string-trim astr)
  (string-trim-left (string-trim-right astr)))

;;=================================

(define (string-prefix? a b)
  (if (<= (string-length a) (string-length b))
      (if (equal? (substring b 0 (string-length a)) a)
          #t
          #f)
      #f))

;;=================================

(define (string-suffix? a b)
  (if (<= (string-length a) (string-length b))
      (if (equal? (substring b (- (string-length b) (string-length a)) (string-length b)) a)
          #t
          #f)
      #f))

;;=================================

(define (string-infix? a b)
  (if (<= (string-length a) (string-length b))
      (if (string-prefix? a b)
          #t
          (string-infix? a (substring b 1)))
      #f))

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





;;================================== ACHIEVMENTS ==================================;;

(define call/cc call-with-current-continuation)

(define *env* #f)
(define *env1* #f)
(define ie (interaction-environment))
(define (ass) (call/cc (lambda (cc) (set! *env* cc))))
(ass)


(define (my-flatten lst)
  (define (loop lis res)
    (cond ((null? lis) res)
          ((pair? lis) (loop (car lis) (loop (cdr lis) res)))
          (else (cons lis res))))
  (loop lst '()))

(define xr '(1 (2 3) #\space #\space #\return #\newline))


(define (only-whitespace-in-list? xs)
  (if (call/cc (lambda (cc)
                 (begin
                   (set! *env* cc)
                   (map (lambda (x)
                          (or (and (char? x) (char-whitespace? x))
                              (*env* #f)))
                        xs))))
      #t
      #f))
                 
(define (list-trim-right xs)
  (define lxs xs)
  (define lout '())
  (call/cc (lambda (tt)
             (begin
               (set! *env1* tt)
               (map (lambda (x)
                      (cond ((only-whitespace-in-list? lxs) (*env1* #f))
                            (else (set! lout (append  lout `(,x)))
                                  x))
                      (set! lxs (cdr lxs))) xs))))
  lout)




(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)))))

#|(define (my-fold-left op xs)
  (let ((stack (car xs)))
    (map (lambda (x)
           (trace-ex (op stack x))) (cdr xs))
    stack)) |#

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))


((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)


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

(define *break* #f)

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
        


               
        
    
    

((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)  


           
  

