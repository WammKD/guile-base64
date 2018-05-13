(use-modules (rnrs bytevectors) (srfi srfi-1))

(define BASE_64_CHARS (string->utf8 (string-append
                                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                                      "ghijklmnopqrstuvwxyz0123456789+/")))
(define TOKEN_LENGTH  3)

(define (base64-encode stringToEncode)
  (define stringLength  (string-length stringToEncode))

  (define padCount      (modulo stringLength TOKEN_LENGTH))
  (define encodedString (make-bytevector padCount (char->integer #\nul)))
  (define paddingString (make-bytevector padCount (char->integer #\=)))

  (iota (ceiling (/ stringLength TOKEN_LENGTH)) 0 TOKEN_LENGTH))
