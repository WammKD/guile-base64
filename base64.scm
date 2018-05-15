(use-modules (rnrs bytevectors) (srfi srfi-1))

(define BASE_64_CHARS (string-append
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                        "ghijklmnopqrstuvwxyz0123456789+/"))
(define TOKEN_LENGTH  3)
(define (string-ref->ASCII str k)
  (char->integer (string-ref str k)))
(define (int->base64-char i)
  (string-ref BASE_64_CHARS (logand i 63)))

(define (base64-encode stringToEncode)
  (define padCount      (modulo
                          (- TOKEN_LENGTH (modulo
                                            (string-length stringToEncode)
                                            TOKEN_LENGTH))
                          TOKEN_LENGTH))

  (define paddingString (make-string padCount #\=))
  (define paddedString  (string-append
                          stringToEncode
                          (make-string padCount #\nul)))
  (define final (fold
                  (lambda (index encodedString)
                    (define result (string-append
                                     encodedString
                                     (if (and
                                           (> index 0)
                                           (= (modulo
                                                (* (/ index TOKEN_LENGTH) 4)
                                                76) 0))
                                         "\r\n"
                                       "")))
                    (define n      (+
                                     (ash (string-ref->ASCII
                                            paddedString
                                            index)            16)
                                     (ash (string-ref->ASCII
                                            paddedString
                                            (1+ index))        8)
                                     (string-ref->ASCII
                                       paddedString
                                       (+ index 2))))

                    (string-append
                      result
                      (list->string (list
                                      (int->base64-char (ash n -18))
                                      (int->base64-char (ash n -12))
                                      (int->base64-char (ash n -6))
                                      (int->base64-char n)))))
                  ""
                  (iota
                    (ceiling (/ (string-length paddedString) TOKEN_LENGTH))
                    0
                    TOKEN_LENGTH)))

  (string-append
    (substring final 0 (- (string-length final) (string-length paddingString)))
    paddingString))
