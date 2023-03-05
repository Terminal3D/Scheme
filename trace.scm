(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex x)
     (begin
       (write 'x)
       (write '=>)
       (let ((outp x))
         (write outp)
         (newline)
         outp)))))
