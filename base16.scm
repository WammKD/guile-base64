(define-module (encode base16)
  #:use-module (rnrs bytevectors)
  #:autoload   (srfi srfi-1) (fold)
  #:export     (base16-encode base16-decode))

(define (base16-encode bvToEncode)
  (fold
    (lambda (num final)
      (string-append
        final
        (let helper ([n num] [return ""])
          (define division  (/ n 16.0))
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

          (if (zero? result)
              (string-append
                (if (= (string-length newReturn) 1) "0" "")
                newReturn)
            (helper result newReturn)))))
    ""
    (bytevector->u8-list bvToEncode)))

(define (base16-decode stringToDecode)
  (define (convertBase16 s)
    (case (car (char-set->list (string->char-set s)))
      [(#\A) 10] [(#\B) 11] [(#\C) 12]
      [(#\D) 13] [(#\E) 14] [(#\F) 15] [else (string->number s)]))

  (define stringLengthHalved (/ (string-length stringToDecode) 2))

  (if (not (integer? stringLengthHalved))
      (error "The given String is not Base16 encoded.")
    (let ([bvToReturn (make-bytevector stringLengthHalved)])
      (for-each
        (lambda (index)
          (bytevector-u8-set!
            bvToReturn
            (/ (- index 2) 2)
            (+
              (* (convertBase16
                   (string-upcase
                     (substring stringToDecode (- index 2) (1- index)))) 16)
              (convertBase16
                (string-upcase (substring stringToDecode (1- index) index))))))
        (iota stringLengthHalved 2 2))

      bvToReturn)))
