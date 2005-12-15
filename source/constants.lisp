;; -*- lisp -*-

(in-package :rfc2388)

;;;; **** Constants corresponding to various ascii characters

(defconstant +Tab+ 9
  "Integer value corresponding to #\\
Tab in ASCII.")

(defconstant +Space+ 32
  "Integer value corresponding to #\\Space in ASCII.")

(defconstant +CR+ 13
  "Integer value corresponding to #\\Return in ASCII.")

(defconstant +LF+ 10
  "Integer value corresponding to #\\Linefeed in ASCII.")

(defconstant +dash+ 45
  "Integer value corresponding to #\\- in ASCII.")

(defconstant +colon+ 58
  "Integer value corresponding to #\\: in ASCII.")

(defconstant +semi-colon+ 59
  "Integer value corresponding to #\= in ASCII.")

(defconstant +backslash+ 92
  "Integer value corresponding to #\\\\ in ASCII.")

(defconstant +equal+ 61
  "Integer value corresponding to #\\= in ASCII.")

(defconstant +double-quote+ 34
  "Integer value corresponding to #\\\" in ASCII.")

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
