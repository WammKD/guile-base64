(use-modules (rnrs bytevectors) (srfi srfi-1))

(define BASE_64_CHARS (bytevector->u8-list
                        (string->utf8 (string-append
                                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                                        "ghijklmnopqrstuvwxyz0123456789+/"))))


(define (bytevector-map-m-to-l m l orig padLength)
  (define (&255             i)   (logand i 255))
  (define (invert           j k) (- (1- k) j))
  (define (int->base64-char i)
    (integer->char (list-ref BASE_64_CHARS (logand i 63))))
  (define (base64-char-ref->int str i)
    (list-index (lambda (int)
                  (= int (char->integer (string-ref str i)))) BASE_64_CHARS))

  (define   origLength ((if (= m 3) length      string-length)   orig))
  (define resultLength (- (* (ceiling (/ origLength m)) l) padLength))
  (define result       ((if (= m 3) make-string make-bytevector) resultLength))

  (define    ref-fun   (if (= m 3) list-ref         base64-char-ref->int))
  (define logand-fun   (if (= m 3) int->base64-char &255))
  (define    set-fun!  (if (= m 3) string-set!      bytevector-u8-set!))

  (do ([increment 0 (+ increment m)]
       [index     0 (+ index     l)])
      [(= increment origLength)]
    (when (and (= m 3) (> index 0) (= (modulo (* (/ increment 3) 4) 76) 0))
      (set! result (string-append
                     (substring result 0     index)
                     "\r\n"
                     (substring result index)))
      (set! index         (+ index        2))
      (set! resultLength  (+ resultLength 2)))

    (let ([n (fold
               (lambda (i sum)
                 (+ sum (ash
                          (ref-fun orig         (+ increment i))
                          (*       (invert i m) (* l         2)))))
               0
               (iota m))])
      (for-each
        (lambda (i)
          (let ([newI (+ index (invert i l))])
            (when (< newI resultLength)
              (set-fun! result newI (logand-fun (ash n (* i (* m -2))))))))
        (iota l))))

  result)



(define (base64-encode bvToEncode)
  (define padCount       (modulo
                           (- 3 (modulo (bytevector-length bvToEncode) 3))
                           3))
  (define bvToListPadded (append
                           (bytevector->u8-list bvToEncode)
                           (make-list padCount (char->integer #\nul))))

  (string-append
    (bytevector-map-m-to-l 3 4 bvToListPadded padCount)
    (make-string padCount #\=)))



(define (base64-decode stringToDecode)
  (define stripped (string-filter
                     (lambda (char) (or (char=? char #\=) (member
                                                            (char->integer char)
                                                            BASE_64_CHARS)))
                     stringToDecode))
  (define stripLen (string-length stripped))

  (define padding  (if (char=? (string-ref stripped (1- stripLen))  #\=)
                       (if (char=? (string-ref stripped (- stripLen 2)) #\=)
                           "AA"
                         "A")
                     ""))
  (define padCount (string-length padding))

  (define updated  (string-append
                     (substring stripped 0 (- stripLen padCount))
                     padding))

  (bytevector-map-m-to-l 4 3 updated padCount))
