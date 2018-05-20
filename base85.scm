(define-module (encode base85)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export     (base85-encode base85-decode))

(define BANG (char->integer #\!))

(define (base85-encode bvToEncode)
  (define bvLength (bytevector-length bvToEncode))

  (define (calc-n index listRange)
    (fold
      (lambda (i final)
        (logior final (ash
                        (logand (bytevector-u8-ref bvToEncode (+ index i)) 255)
                        (* (- 3 i) 8))))
      0
      (iota listRange)))
  (define (gen-chars num rangeList)
    (fold
      (lambda (i result)
        (string-append result (list->string
                                (list (integer->char
                                        (+ BANG (modulo
                                                  (floor (/ num (expt 85 i)))
                                                  85)))))))
      ""
      rangeList))

  (string-append
    (string-append
      (fold
        (lambda (increment result)
          (string-append result (let ([n (calc-n increment 4)])
                                  (if (= n 0)
                                      "z"
                                    (gen-chars n (reverse (iota 5)))))))
        "<~"
        (iota (floor (/ bvLength 4)) 0 4))
      (let* ([lengthResidue (modulo bvLength 4)]
             [n             (calc-n (- bvLength lengthResidue) lengthResidue)])
        (gen-chars n (if (zero? lengthResidue)
                         '()
                       (reverse (list-tail (iota 5) (- 4 lengthResidue)))))))
    "~>"))
