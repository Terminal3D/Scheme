(load "homework4.scm")
(load "unit-test.scm")


(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ВЫПОЛНЯЕТ ТЕСТЫ ДЛЯ АТД=-\n\n")


(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s #f)
(define r #f)
(define t #f)
(define c #f)

;; Приближённое значение числа Пи, рекомендуемое Архимедом.
;; См.: https://ru.wikipedia.org/wiki/Пи_(число)#История
;; Это рациональное число, оно точное (exact), с ним ошибок округления не бывает.
(define PI 22/7)

(define (perim f)
  (match f
    ((square a) (* 4 a))
    ((rectangle a b) (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r) (* 2 PI r))))

(define (scale f x)
  (match f
    ((square a) (square (* a x)))
    ((rectangle a b) (rectangle (* a x) (* b x)))
    ((triangle a b c) (triangle (* a x) (* b x) (* c x)))
    ((circle r) (circle (* r x)))))


(define-data the-list ((the-nil)
                       (the-cons head tail)))

(define (the-length xs)
  (match xs
    ((the-nil) 0)
    ((the-cons hd tl) (+ 1 (the-length tl)))))

(define (the-prod xs)
  (match xs
    ((the-nil) 1)
    ((the-cons hd tl)
     (match tl
       ((the-nil) hd)
       ((the-cons _1 _2) (* hd (the-prod tl)))))))

(define the-xs #f)


(define tests
  (list (test (figure? #t) #f)
        (test (figure? #f) #f)
        (test (figure? 'hello) #f)
        (test (figure? 888) #f)
        (test (figure? '()) #f)
        (test (figure? #()) #f)
        (test (figure? '(one two three)) #f)
        (test (figure? #(one two three)) #f)

        (test (begin (set! s (square 100))
                     (figure? s))
              #t)
        (test (begin (set! r (rectangle 100 200))
                     (figure? r))
              #t)
        (test (begin (set! t (triangle 300 400 500))
                     (figure? t))
              #t)
        (test (begin (set! c (circle 100))
                     (figure? t))
              #t)

        (test (the-list? s) #f)
        (test (the-list? r) #f)
        (test (the-list? t) #f)
        (test (the-list? c) #f)

        (test (perim s) 400)
        (test (perim r) 600)
        (test (perim t) 1200)
        (test (perim c) 4400/7)

        (test (perim (scale s 1/100)) 4)
        (test (perim (scale r 1/100)) 6)
        (test (perim (scale t 1/100)) 12)
        (test (perim (scale c 1/100)) 44/7)

        (test (the-list? #t) #f)
        (test (the-list? #f) #f)
        (test (the-list? 'hello) #f)
        (test (the-list? 888) #f)
        (test (the-list? '()) #f)
        (test (the-list? #()) #f)
        (test (the-list? '(one two three)) #f)
        (test (the-list? #(one two three)) #f)

        (test (begin (set! the-xs
                           (the-cons 7 (the-cons 11 (the-cons 13 (the-nil)))))
                     (the-list? the-xs))
              #t)
        (test (figure? the-xs) #f)

        (test (the-length the-xs) 3)
        (test (the-prod the-xs) 1001)))


(define **test-succeed-32476** (run-tests tests))
