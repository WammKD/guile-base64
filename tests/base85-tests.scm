(load "../base85.scm")

(use-modules (srfi srfi-64) (encode base85) (rnrs bytevectors))

(define (string->bv-and-encode str)
  (base85-encode (string->utf8 str)))
(define (decode-string->string str)
  (utf8->string (base85-decode str)))


(test-begin "Encode")

(test-assert
    "Guile Website"
  (string=?
    "<~GB\\6`B5ViEDfTJDF)PZ4G@>N'05#EACh3~>"
    (string->bv-and-encode "www.gnu.org/software/guile")))

(test-assert "Empty" (string=? "<~~>"     (base85-encode #vu8())))
(test-assert "a"     (string=? "<~@/~>"   (base85-encode #vu8(97))))
(test-assert "ab"    (string=? "<~@:B~>"  (base85-encode  #u8(97 98))))
(test-assert "abc"   (string=? "<~@:E^~>" (base85-encode  #s8(97 98 99))))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "<~@:E_WAS,RgBkhF\"D/O92EH6,BF`qtRH$VbC6UX@47n?3D92&&T:"
      "Jand;cHat='/U/0JP==1c70M3&r-I,;<FN.OZ`-3]oSW/g+A(H[P~>")
    (string->bv-and-encode (string-append
                             "abcdefghijklmnopqrstuvwxyz"
                             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             "0123456789!@#0^&*();:<>,. []{}"))))

(test-end "Encode")



(test-begin "Decode")

(test-assert
    "Guile Website"
  (string=?
    "www.gnu.org/software/guile"
    (decode-string->string "<~GB\\6`B5ViEDfTJDF)PZ4G@>N'05#EACh3~>")))

(test-assert "Empty" (bytevector=? #vu8()         (base85-decode "<~~>")))
(test-assert "a"     (string=? "a"  (utf8->string (base85-decode "@/"))))
(test-assert "ab"    (string=? "ab" (utf8->string (base85-decode "<~@:B~>"))))
(test-assert "abc"   (bytevector=? #vu8(97 98 99) (base85-decode "@:E^")))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "0123456789!@#0^&*();:<>,. []{}")
    (decode-string->string (string-append
                             "<~@:E_WAS,RgBkhF\"D/O92EH6,BF`qtRH$V"
                             "bC6UX@47n?3D92&&T:Jand;cHat='/U/0JP="
                             "=1c70M3&r-I,;<FN.OZ`-3]oSW/g+A(H[P~>"))))

(test-end "Decode")



(test-begin "End to End")

(test-assert
    "Nul Character"
  (string=? "\x00"   (decode-string->string (string->bv-and-encode "\x00"))))
(test-assert
    "abcd"
  (string=? "abcd"   (decode-string->string (string->bv-and-encode "abcd"))))
(test-assert
    "abcde"
  (string=? "abcde"  (decode-string->string (string->bv-and-encode "abcde"))))
(test-assert
    "abcdef"
  (string=? "abcdef" (decode-string->string (string->bv-and-encode "abcdef"))))
(test-assert
    "Truth"
  (bytevector=?
    #vu8(68 97 110 105 32 105 115 32 67  117 116 101 226 132 162
         32 97 110 100 32 65  109 97 122 105 110 103 226 132 162 46)
    (base85-decode (base85-encode #vu8(68  97  110 105 32  105 115 32  67  117
                                       116 101 226 132 162 32  97  110 100 32
                                       65  109 97  122 105 110 103 226 132 162 46)))))

(test-end "End to End")
