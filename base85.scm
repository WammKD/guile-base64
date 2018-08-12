(define-module (encode base85)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export     (base85-encode base85-decode))

(define FOUR     4)
(define FIVE     5)
(define BANG     (char->integer #\!))
(define (&255 i) (logand i 255))

(define (base85-encode bvToEncode)
  (define bvLength (bytevector-length bvToEncode))

  (define (calc-n index listRange)
    (fold
      (lambda (i final)
        (logior final (ash
                        (&255 (bytevector-u8-ref bvToEncode (+ index i)))
                        (* (- (1- FOUR) i) 8))))
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
          (string-append result (let ([n (calc-n increment FOUR)])
                                  (if (= n 0)
                                      "z"
                                    (gen-chars n (reverse (iota FIVE)))))))
        "<~"
        (iota (floor (/ bvLength FOUR)) 0 FOUR))
      (let* ([lengthResidue (modulo bvLength FOUR)]
             [n             (calc-n (- bvLength lengthResidue) lengthResidue)])
        (gen-chars n (if (zero? lengthResidue)
                         '()
                       (reverse (list-tail (iota FIVE) (- FOUR lengthResidue)))))))
    "~>"))

(define (base85-decode stringToDecode)
  (define newStD       (if (string= (substring stringToDecode 0 2) "<~")
                           (substring stringToDecode 2 (- (string-length
                                                            stringToDecode) 2))
                         stringToDecode))
  (define stringLength (    string-length newStD))
  (define result       (make-bytevector (floor (/ (* stringLength FOUR) FIVE))))
  (define     bvLength (bytevector-length         result))

  (define (decodeChunk str bv bvi)
    (let ([n (fold
               (lambda (i final)
                 (+ final (*
                            (- (char->integer (string-ref str i)) BANG)
                            (expt 85 (- (1- FIVE) i)))))
               0
               (iota FIVE 0))])
      (for-each
        (lambda (j)
          (when (< (+ bvi j) bvLength)
            (bytevector-u8-set! bv (+ bvi j) (&255
                                               (ash n (* (- (1- FOUR) j) -8))))))
        (iota FOUR 0))))

  (do ([stringIndex 0 (+ stringIndex FIVE)]
       [  byteIndex 0 (+   byteIndex FOUR)])
      [(>= (+ stringIndex (1- FIVE)) stringLength)]
    (let ([chunk (substring newStD stringIndex (+ stringIndex FIVE))])
      (cond
       [(string-contains chunk "z" 1)   (error       (string-append
                                                       "The given String is "
                                                       "not Base85 encoded."))]
       [(string-contains chunk "z" 0 1) (decodeChunk "!!!!!" result byteIndex)]
       [else                            (decodeChunk   chunk result byteIndex)])))

  (when (not (= (modulo bvLength FOUR) 0))
    (let ([strRemaining (remainder stringLength FIVE)])
      (decodeChunk
        (string-append
          (substring   newStD                (- stringLength strRemaining))
          (make-string (- FIVE strRemaining) #\u))
        result
        (- bvLength (remainder bvLength FOUR)))))

  result)
