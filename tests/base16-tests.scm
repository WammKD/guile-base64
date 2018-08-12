(load "../base16.scm")

(use-modules (srfi srfi-64) (encode base16) (rnrs bytevectors))

(define (string->bv-and-encode str)
  (base16-encode (string->utf8 str)))
(define (decode-string->string str)
  (utf8->string (base16-decode str)))


(test-begin "Encode")

(test-assert
    "Guile Website"
  (string=?
    "7777772E676E752E6F72672F736F6674776172652F6775696C65"
    (string->bv-and-encode "www.gnu.org/software/guile")))

(test-assert "Empty" (string=? ""       (base16-encode #vu8())))
(test-assert "a"     (string=? "61"     (base16-encode #vu8(97))))
(test-assert "ab"    (string=? "6162"   (base16-encode  #u8(97 98))))
(test-assert "abc"   (string=? "616263" (base16-encode  #s8(97 98 99))))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "6162636465666768696A6B6C6D6E6F707172737475767778797A41"
      "42434445464748494A4B4C4D4E4F505152535455565758595A3031"
      "3233343536373839214023305E262A28293B3A3C3E2C2E205B5D7B7D")
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
    (decode-string->string
      "7777772E676E752E6F72672F736F6674776172652F6775696C65")))

(test-assert "Empty" (bytevector=? #vu8()         (base16-decode "")))
(test-assert "a"     (bytevector=? #vu8(97)       (base16-decode "61")))
(test-assert "ab"    (string=? "ab" (utf8->string (base16-decode "6162"))))
(test-assert "abc"   (bytevector=? #vu8(97 98 99) (base16-decode "616263")))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "0123456789!@#0^&*();:<>,. []{}")
    (decode-string->string (string-append
                             "6162636465666768696A6b6C6d6E6f70717273747"
                             "5767778797A4142434445464748494a4B4C4D4E4F"
                             "505152535455565758595A3031323334353637383"
                             "9214023305E262a28293B3A3c3E2C2e205B5D7B7d"))))

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
    (base16-decode (base16-encode #vu8(68  97  110 105 32  105 115 32  67  117
                                       116 101 226 132 162 32  97  110 100 32
                                       65  109 97  122 105 110 103 226 132 162 46)))))

(test-end "End to End")
