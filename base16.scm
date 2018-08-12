(define-module (encode base16)
  #:use-module (rnrs bytevectors)
  #:autoload   (srfi srfi-1) (fold)
  #:export     (base16-encode base16-decode))

(define (base16-encode bvToEncode)
  (define (get-base-10 m)
    (let helper ([l m] [start 1])
      (if (< l start) start (helper l (* start 10)))))

  (fold
    (lambda (num final)
      (string-append
        final
        (let help ([p num] [return ""])
          (define division  (/ p 16.0))
          (define result    (floor division))
          (define newReturn (string-append
                              (let ([remainder (inexact->exact
                                                 (* (- division result) 16))])
                                (case remainder
		                  [(10) "A"] [(11) "B"]
                                  [(12) "C"] [(13) "D"]
                                  [(14) "E"] [(15) "F"] [else (number->string
                                                                remainder)]))
                              return))

          (if (zero? result) newReturn (help result newReturn)))))
    ""
    (bytevector->u8-list bvToEncode)))
