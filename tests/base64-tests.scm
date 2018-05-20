(load "../base64.scm")

(use-modules (srfi srfi-64) (encode base64) (rnrs bytevectors))

(define (string->bv-and-encode str)
  (base64-encode (string->utf8 str)))
(define (decode-string->string str)
  (utf8->string (base64-decode str)))


(test-begin "Encode")

(test-assert
    "Guile Website"
  (string=?
    "d3d3LmdudS5vcmcvc29mdHdhcmUvZ3VpbGU="
    (string->bv-and-encode "www.gnu.org/software/guile")))

(test-assert "Empty" (string=? ""     (base64-encode #vu8())))
(test-assert "a"     (string=? "YQ==" (base64-encode #vu8(97))))
(test-assert "ab"    (string=? "YWI=" (base64-encode  #u8(97 98))))
(test-assert "abc"   (string=? "YWJj" (base64-encode  #s8(97 98 99))))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUV"
      "JTVFVWV1hZWjAxMjM0\r\nNTY3ODkhQCMwXiYqKCk7Ojw+LC4gW117fQ==")
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
    (decode-string->string "d3d3LmdudS5vcmcvc29mdHdhcmUvZ3VpbGU=")))

(test-assert "Empty" (bytevector=? #vu8()         (base64-decode "")))
(test-assert "a"     (bytevector=? #vu8(97)       (base64-decode "YQ==")))
(test-assert "ab"    (string=? "ab" (utf8->string (base64-decode "YWI="))))
(test-assert "abc"   (bytevector=? #vu8(97 98 99) (base64-decode "YWJj")))

(test-assert
    "Lots o' Characters"
  (string=?
    (string-append
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "0123456789!@#0^&*();:<>,. []{}")
    (decode-string->string (string-append
                             "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQk"
                             "NERUZHSElKS0xNTk9QUVJTVFVWV1hZWjAxMjM0"
                             "\r\nNTY3ODkhQCMwXiYqKCk7Ojw+LC4gW117fQ=="))))

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
    (base64-decode (base64-encode #vu8(68  97  110 105 32  105 115 32  67  117
                                       116 101 226 132 162 32  97  110 100 32
                                       65  109 97  122 105 110 103 226 132 162 46)))))

(test-end "End to End")
