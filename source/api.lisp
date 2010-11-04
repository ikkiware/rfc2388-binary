;; -*- lisp -*-

#+xcvb
(module
 (:depends-on ("source/packages")))

(in-package :rfc2388-binary)

;;;; ** Public Interface

(defgeneric read-mime (source boundary callback)
  (:documentation
   "Parses the MIME entites in SOURCE.

SOURCE is either a vector of (unsigned-byte 8) or a stream whose
element-type is (unsigned-byte 8). BOUNDARY is either a string of
US-ASCII encodeable characters or a byte vector. CALLBACK is a
function which will be passed one argument, a MIME-PART
containing the headers of the mime part and must return the following
values:

- a byte-handler function. This is a one argument function which
  will be passed every byte in the mime part's content.

- a termination function. This is a function without arguments
  and will be called when the operation finishes without errors.
  It must return whatever is to be returned from read-mime.

- an optional abort function. This is a function without arguments
  and will be called when the operation is aborted due to an error.

READ-MIME consumes bytes from SOURCE and returns a list of the
whatever the various termination functions returned.

Example:

 (read-mime #<a binary stream> \"123\"
            (lambda (mime-parte)
              (values (lambda (byte) (collect-byte-somewhere byte))
                      (lambda (mime-part) mime-part))))

  This call would return a list of mime-part objects passing each
byte to collect-byte-somewhere.

You may also want to look at UCW for a real-world example."))

(defclass mime-part ()
  ((content :accessor content :initform nil)
   (content-length :accessor content-length :initform nil)
   (content-type :accessor content-type :initform nil)
   (content-charset :accessor content-charset :initform nil)
   (headers :accessor headers :initform '())))

(defun print-mime-part (part &optional (stream *trace-output*))
  (check-type part mime-part)
  (format stream "Headers:~%")
  (dolist (header (headers part))
    (format stream "~S: ~S~:{; ~S=~S~}~%"
            (header-name header) (header-value header)
            (mapcar (lambda (attribute)
                      (list (car attribute) (cdr attribute)))
                    (header-attributes header))))
  (format stream "~%Content:~%")
  (princ (content part) stream)
  (format stream "~%"))

(defgeneric get-header (part header-name)
  (:documentation "Returns the mime-header object for the header named HEADER-NAME (a string)."))

(defmethod get-header ((part mime-part) (header-name string))
  (find header-name (headers part)
        :key #'header-name
        :test #'string=))

(defclass mime-header ()
  ((name :accessor header-name
         :initarg :name)
   (value :accessor header-value
          :initarg :value)
   (attributes :accessor header-attributes
               :initarg :attributes)))

(defgeneric get-header-attribute (header name)
  (:documentation "Returns the value of the attribute named NAME
  in the header HEADER."))

(defmethod get-header-attribute ((header mime-header) (name string))
  (cdr (assoc name (header-attributes header) :test #'string-equal)))

(defun parse-mime (source boundary
                   &key write-content-to-file
                   (byte-encoder #'code-char))
  "Parses MIME entities, returning them as a list.

Each element in the list is of form: (body headers), where BODY is the
contents of MIME part, and HEADERS are all headers for that part.
BOUNDARY is a string used to separate MIME entities.

This is the convenience interface to READ-MIME, all data is read
into memory or a file and we assume that every byte in the data
corresponds to exactly one character.

The SOURCE and BOUNDARY arguments are passed unchanged to
READ-MIME. See READ-MIME's documentation for details."
  (check-type write-content-to-file (or null string pathname))
  (read-mime source boundary
             (if write-content-to-file
                 (make-mime-file-writer write-content-to-file
                                        :byte-encoder byte-encoder)
                 (make-mime-buffer-writer :byte-encoder byte-encoder))))

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
