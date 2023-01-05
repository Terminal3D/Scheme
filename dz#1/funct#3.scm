(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))

(define (my-lcm a b)
  (* (/ a (my-gcd a b)) b))


(define (find-divisor n test-divisor)
  (cond ((> test-divisor (/ n 2)) n)
        ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (prime? n)
  (= n (find-divisor n 2)))


            
           