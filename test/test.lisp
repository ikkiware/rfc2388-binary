;; -*- lisp -*-

(in-package :rfc2388.test)

(def-suite :rfc2388)

(in-suite :rfc2388)

(test parse-key-values
  (macrolet ((test-key-values (bind string &body body)
               `(destructuring-bind ,bind
                    (rfc2388::parse-key-values ,string)
                  ,@body)))
    (test-key-values ((foo . bar)) "foo=bar"
      (is (string= "foo" foo))
      (is (string= "bar" bar)))

    (test-key-values ((one . two) (three . four))
                      "one=two;three=four"
      (is (string= "one" one))
      (is (string= "two" two))
      (is (string= "three" three))
      (is (string= "four" four)))))

(test as-ascii-char
  (is (char= #\Space (rfc2388::as-ascii-char 32)))
  (is (char= #\Tab (rfc2388::as-ascii-char 9)))
  (is (char= #\! (rfc2388::as-ascii-char 33)))
  (is (char= #\: (rfc2388::as-ascii-char 58)))
  (is (char= #\a (rfc2388::as-ascii-char 97)))
  (is (char= #\A (rfc2388::as-ascii-char 65))))

(test empty-data
  (with-input-from-file (mime "data/mime1" :element-type '(unsigned-byte 8))
    (is-true
     (rfc2388::read-until-next-boundary mime
                                        (rfc2388::ascii-string-to-boundry-array "12345678")
                                        (lambda (byte)
                                          (declare (ignore byte))
                                          (fail)))))
  (with-input-from-file (mime "data/mime2" :element-type '(unsigned-byte 8))
    (is-false
     (rfc2388::read-until-next-boundary mime
                                        (rfc2388::ascii-string-to-boundry-array "12345678")
                                        (lambda (byte)
                                          (fail "Read char byte ~D (~C), why?" byte (code-char byte)))))))

(test hello-world
  (with-output-to-string (hello-world)
    (with-input-from-file (mime "data/mime3" :element-type '(unsigned-byte 8))
      (is-true
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundry-array "12345678")
                                          (lambda (byte)
                                            (write-char (code-char byte) hello-world))))
      (is (string= "hello, world!" (get-output-stream-string hello-world)))))
  (with-output-to-string (hello-world)
    (with-input-from-file (mime "data/mime4" :element-type '(unsigned-byte 8))
      (is-true
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundry-array "12345678")
                                          (lambda (byte)
                                            (declare (ignore byte))
                                            (fail))))
      (is-false
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundry-array "12345678")
                                          (lambda (byte)
                                            (write-char (code-char byte) hello-world))))
      (is (string= "hello, world!" (get-output-stream-string hello-world))))))

(test parse-header
  (with-input-from-file (header "data/header1"
                                :element-type '(unsigned-byte 8))
    (multiple-value-bind (found-header header-name header-value)
        (rfc2388::read-next-header header)
      (is-true found-header)
      (is (string= "foo" header-name))
      (is (string= "bar" header-value)))
    (is-false (rfc2388::read-next-header header))))

(test parse-mime
  (with-input-from-file (mime "data/mime5" :element-type '(unsigned-byte 8))
    (let ((parts (read-mime mime "AaB03x")))
      (let ((larry (first parts)))
        (is (equalp (content larry)
                    (map-into (make-array 5 :element-type '(unsigned-byte 8))
                              #'char-code
                              (list #\L #\a #\r #\r #\y)))))
      (let ((file1 (second parts)))
        (is (equalp (content file1)
                    (map-into (make-array 9 :element-type '(unsigned-byte 8))
                              #'char-code
                              (list #\f #\i #\l #\e #\1 #\. #\t #\x #\t))))
        (is (string= "text/plain" (content-type file1))))
      (is (= 2 (length parts))))))

;; Copyright (c) 2003 Janis Dzerins
;; Copyright (c) 2005 Edward Marco Baringer
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
