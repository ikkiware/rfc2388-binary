;; -*- lisp -*-
(in-package :rfc2388-binary)

(declaim (inline linear-whitespace-byte? as-ascii-char))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug* nil
    "When T we compile the code with some logging statements built in."))

(defmacro debug-message (message &rest message-control)
  (when *debug*
    `(format *debug-io* ,message ,@message-control)))

(defun linear-whitespace-byte? (byte)
  "In short: is it a space or a tab?"
  (or (= 32 byte)
      (= 9 byte)))

(defun as-ascii-char (byte)
  "Assuming BYTE is an ASCII coded character retun the corresponding character."
  (cond
    ((eq 32 byte)  #\Space)
    ((eq 9 byte) #\Tab)
    ((or (> byte 127)
	 (< byte 33))
     ;; FIXME implement rfc2231. meanwhile we simply get rid of non-ascii characters...
     (debug-message "Non-ascii chars found in request, filename may be incorrect.~%")
     #\X)
    (t
     ;; here we only have bytes from the ASCII range, so CODE-CHAR does the right thing
     (code-char byte))))

(defun ascii-string-to-boundary-array (string)
  (map-into (make-array (length string)
                        :element-type '(unsigned-byte 8)
                        :adjustable nil)
            (lambda (char)
              (if (< (char-code char) 128)
                  (char-code char)
                  (error "Bad character ~C in a MIME boundary ~S" char string)))
            string))

;; Copyright (c) 2003 Janis Dzerins
;; Modifications for TBNL Copyright (c) 2004 Michael Weber and Dr. Edmund Weitz
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
