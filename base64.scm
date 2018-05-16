(use-modules (rnrs bytevectors) (srfi srfi-1))

(define BASE_64_CHARS (bytevector->u8-list
                        (string->utf8 (string-append
                                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                                        "ghijklmnopqrstuvwxyz0123456789+/"))))
(define TOKEN_LENGTH  3)

(define (int->base64-byte i)
  (list-ref BASE_64_CHARS (logand i 63)))



(define (base64-encode bvToEncode)
  (define padCount       (modulo
                           (- TOKEN_LENGTH (modulo
                                             (bytevector-length bvToEncode)
                                             TOKEN_LENGTH))
                           TOKEN_LENGTH))

  (define equalsPadding  (make-list padCount (char->integer #\=)))
  (define bvToListPadded (append
                           (bytevector->u8-list bvToEncode)
                           (make-list padCount (char->integer #\nul))))

  (define final
    (fold
      (lambda (index encodedList)
        (define result (if (and
                             (> index 0)
                             (= (modulo (* (/ index TOKEN_LENGTH) 4) 76) 0))
                           (cons                ; \n
                             (char->integer #\return)  ; \r
                             (cons (char->integer #\newline) encodedList))
                         encodedList))
        (define n      (+
                         (ash (list-ref bvToListPadded      index) 16)
                         (ash (list-ref bvToListPadded (1+ index))  8)
                         (list-ref bvToListPadded (+ index 2))))

        (append
          (list (int->base64-byte n)           (int->base64-byte (ash n -6))
                (int->base64-byte (ash n -12)) (int->base64-byte (ash n -18)))
          result))
      '()
      (iota (ceiling (/ (length bvToListPadded) TOKEN_LENGTH)) 0 TOKEN_LENGTH)))

  (utf8->string (list->u8vector (reverse (append
                                           equalsPadding
                                           (drop final (length equalsPadding)))))))



(define (base64-decode stringToDecode)
  (define stripped (string-filter
                     (lambda (char) (member (char->integer char) BASE_64_CHARS))
                     stringToDecode))
  (define stripLen (string-length stripped))
  (define padding  (if (char=? (string-ref stripped (1- stripLen)) #\=)
                       (if (char=? (string-ref stripped (- stripLen 2)) #\=)
                           "AA"
                         "A")
                     ""))
  (define updated  (string-append
                     (substring stripped 0 (- stripLen (string-length padding)))
                     padding))
  )
