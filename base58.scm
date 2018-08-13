(define-module (encode base58)
  #:use-module (rnrs bytevectors)
  #:export     (base58-encode base58-decode))

(define ZERO          0)
(define FIFTY_EIGHT   58)
(define BASE_58_CHARS (string-append
                        "123456789ABCDEFGHJKLMNPQRSTUV"
                        "WXYZabcdefghijkmnopqrstuvwxyz"))

(define* (base58-encode bvToEncode #:optional [base 16])
  (let loop ([i (string->number
                  (substring
                    (list->string
                      (map integer->char (bytevector->u8-list bvToEncode)))
                    (if (and
                          (= base                              16)
                          (= (bytevector-u8-ref bvToEncode 0)  48)  ; 0
                          (= (bytevector-u8-ref bvToEncode 1) 120)) #|x|# 2 0))
                  base)]
             [stringToReturn ""])
    (if (> i ZERO)
        (loop
          (floor (/ i FIFTY_EIGHT))
          (string-append
            (let ([index (modulo i FIFTY_EIGHT)])
              (substring BASE_58_CHARS index (1+ index)))
            stringToReturn))
      stringToReturn)))

(define* (base58-decode stringToDecode #:optional [base 16])
  (string->utf8
    (number->string
      (string-fold
        (lambda (char final)
          (+ (* final FIFTY_EIGHT) (string-index BASE_58_CHARS char)))
        ZERO
        stringToDecode)
      base)))
