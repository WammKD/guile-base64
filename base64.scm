(use-modules (rnrs bytevectors) (srfi srfi-1))

(define BASE_64_CHARS (string-append
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                        "ghijklmnopqrstuvwxyz0123456789+/"))
(define TOKEN_LENGTH  3)

(define (base64-encode stringToEncode)
  (define stringLength  (string-length stringToEncode))
  (define padCount      (modulo stringLength TOKEN_LENGTH))

  (define paddingString (make-string padCount #\=))
  (define paddedString  (string-append
                          stringToEncode
                          (make-string padCount #\nul)))

  (fold
    (lambda (elem result)
      result)
    ""
    (iota (ceiling (/ stringLength TOKEN_LENGTH)) 0 TOKEN_LENGTH)))
