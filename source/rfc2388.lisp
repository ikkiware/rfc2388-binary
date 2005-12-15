;; -*- lisp -*-

(in-package :rfc2388)

;;;; ** Public Interface

(defgeneric parse-mime (source boundry callback)
  (:documentation
   "Parses the MIME entites in SOURCE.

SOURCE is either a vector of (unsigned-byte 8) or a stream whose
element-type is (unsigned-byte 8). CALLBACK is a function which
will be passed one argument, a MIME-PART containing the headers
of the mime part and must return a one argument function. The
returned function will be called once for every byte of data in
the mime part."))

(defun read-mime (source boundry)
  "Parses the MIME data in SOURCE, returns it as
  a list of MIME-PART objects containing the headers and the
  data.

This is the convenience interface to PARSE-MIME, all data is read
into memory, we assume that every byte in the data corresponds to
exactly one character and that code-char is sufficent to convert
bytes to characters.

The SOURCE and BOUNDRY arguments are passed unchanged to
PARSE-MIME. See PARSE-MIME's documentation for details."
  (parse-mime source boundry
              (lambda (partial-mime-part)
                (setf (content partial-mime-part)
                      (make-array (or (content-length partial-mime-part)
                                      1024)
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0))
                (lambda (byte)
                  (vector-push-extend byte (content partial-mime-part))))))

(defclass mime-part ()
  ((content :accessor content :initform nil)
   (content-length :accessor content-length :initform nil)
   (content-type :accessor content-type :initform nil)
   (content-charset :accessor content-charset :initform nil)
   (headers :accessor headers :initform '())))

(defmethod get-header ((part mime-part) (header-name string))
  (cdr (assoc header-name (headers part) :test #'string-equal)))

;;;; ** Implementation

;;;; *** Actual parsers

(defmethod parse-mime ((source string) boundry callback)
  (with-input-from-string (source source)
    (parse-mime source boundry callback)))

(defmethod parse-mime ((source stream) (boundry string) callback)
  (parse-mime source (ascii-string-to-boundry-array boundry) callback))

(defmethod parse-mime ((source stream) (boundry array) callback)
  ;; read up to the first part
  (read-until-next-boundary source boundry nil :assume-first-boundry t)
  ;; read headrs and bonudies until we're done
  (loop
     for part = (loop
                   named read-headers
                   with part = (make-instance 'mime-part)
                   do (multiple-value-bind (found-header name value)
                          (read-next-header source)
                        (if found-header
                            (cond
                              ((string-equal "Content-Type" name)
                               (multiple-value-bind (content-type attributes)
                                   (parse-header-value value)
                                 (setf (content-type part) content-type)
                                 (let ((charset (cdr (assoc "charset" attributes :test #'string-equal))))
                                   (when charset
                                     (setf (content-charset part) charset)))))
                              ((string-equal "Content-Length" name)
                               (setf (content-length part) value))
                              (t
                               (push (cons name value) (headers part))))
                            (return-from read-headers part))))
     for more = (read-until-next-boundary source boundry (funcall callback part))
     collect part
     while more))

(defun read-until-next-boundary (stream boundary data-handler
                                 &key assume-first-boundry)
  "Reads from STREAM up to the next boundary. For every byte of
data in stream we call DATA-HANDLER passing it the byte. Returns
T if there's more data to be read, NIL otherwise.

The ASSUME-FIRST-BOUNDRY parameter should T if we're reading the
first part of a MIME mesage since there is no leading CR LF
sequence."
  ;; Read until  CR|LF|-|-|boundary|-|-|transport-padding|CR|LF
  ;; States:    0  1  2 3 4        5 6 7                 8  9  10
  ;; States 6 and 7 are optional
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 8) (*)) boundary)
           (type (function ((unsigned-byte 8)) t) data-handler))
  (let ((queued-bytes (make-array 74 :element-type '(unsigned-byte 8)))
        (queue-index 0)
        (boundary-index 0)
        (boundary-length (length boundary))
        (state (if assume-first-boundry
                   2
                   0))
        (byte 0)
        (more-data t))
    (declare (type (simple-array (unsigned-byte 8) (74)) queued-bytes)
             (type (integer 0 74) queue-index)
             (type (integer 0 70) boundary-index)
             (type (integer 1 70) boundary-length)
             (type (integer 0 10) state)
             (type (unsigned-byte 8) byte)
             (type boolean more-data))
    (labels ((handle-byte ()
               #+rfc2388.debug (format t "Handling byte ~D. State is ~D.~%" byte state)
               (funcall data-handler byte))
             (flush-queued-bytes ()
               (dotimes (i queue-index)
                 (setf byte (aref queued-bytes i))
                 (handle-byte))
               (setf queue-index 0))
             (enqueue-byte ()
               (setf (aref queued-bytes queue-index) byte)
               (incf queue-index))
             (parse-next-byte ()
               (setf byte (read-byte stream))
               #+rfc2388.debug (format t "Reading byte ~D (~C). State is ~D.~%" byte (code-char byte) state)
               (case byte
                 (13 ;; Carriage-Return
                  (case state
                    (0 (setf state 1)
                       (enqueue-byte))
                    ((5 7 8)
                     (setf state 9)
                     (enqueue-byte))
                    (t (setf state 0)
                       (flush-queued-bytes)
                       (handle-byte))))
                 (10 ;; Line-Feed
                  (case state
                    (1 (setf state 2)
                       (enqueue-byte))
                    (9 ;; all done.
                     (return-from read-until-next-boundary
                       (values more-data)))
                    (t (setf state 0)
                       (flush-queued-bytes)
                       (handle-byte))))
                 (45 ;; Dash
                  (case state
                    (2 (setf state 3)
                       (enqueue-byte))
                    (3 (setf state 4)
                       (enqueue-byte))
                    (5 (setf state 6)
                       (enqueue-byte))
                    (6 (setf state 7)
                       (setf more-data nil))
                    (t (setf state 0)
                       (flush-queued-bytes)
                       (handle-byte))))
                 (t
                  (cond
                    ((and (or (= 5 state)
                              (= 7 state))
                          (lwsp-char-p byte))
                     ;; transport-padding. do nothing.
                     nil)
                    ((and (= 4 state)
                          (= byte (aref boundary boundary-index)))
                     (incf boundary-index)
                     (enqueue-byte)
                     (when (= boundary-index boundary-length)
                       ;; done with the boundry
                       (setf state 5)))
                    ((and (= 4 state))
                     (setf state 0)
                     (flush-queued-bytes)
                     (handle-byte))
                    (t (setf state 0)
                       (handle-byte)))))))
      (loop
         ;; this loop will exit when one of two conditions occur:
         ;; 1) we hit an EOF in the stream
         ;; 2) we read the next boundry and return. (see the
         ;;    return-from form in the hnadler for the +LF+ char.
         (parse-next-byte)))))

(defun read-next-header (stream)
  "Reads the next header from STREAM. Returns, as the first
  value, T if a header was found and NIL otherwise. When a header
  is found two more values are returned: the header name (a
  string) and the header value (also a string). Headers are
  assumed to be encoded in 7bit ASCII.

The returned strings may actually be displaced arrays."
  ;; another state machine:
  ;; |header-name|:|whitespace|header-value|CR|LF|
  ;; 0             1          2               3 
  ;; |CR|LF
  ;; 0  4
  ;; If we find a CR LF sequence there's no header.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((state 0)
        (byte 0)
        (header-name (make-array 256 :element-type 'character :adjustable t :fill-pointer 0))
        (header-value (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
    (declare (type (integer 0 4) state)
             (type (unsigned-byte 8) byte)
             (type (array character (*)) header-name header-value))
    (labels ((extend (array)
               (vector-push-extend (as-ascii-char byte) array)))
    (loop
       (setf byte (read-byte stream))
       (case byte
         (13 ;; Carriage-Return
          (ecase state
            (0 ;; found a CR. no header
             (setf state 4))
            (2 ;; end of header-value
             (setf state 3))))
         (10 ;; Line-Feed
          (ecase state
            (4 ;; all done. no header
             (return-from read-next-header (values nil nil nil)))
            (3 ;; all done. found header
             (return-from read-next-header (values t header-name header-value)))))
         (58 ;; #\:
          (ecase state
            (0 ;; done reading header-name
             (setf state 1))
            (2 ;; colon in header-value
             (extend header-value))))
         ((32 9) ;; #\Space or #\Tab
          (ecase state
            (1 ;; whitespace after colon.
             nil)
            (2 ;; whitespace in header-value
             (extend header-value))))
         (t
          (ecase state
            (0 ;; character in header-name
             (extend header-name))
            (1 ;; end of whitespace after colon (there may be no whitespace)
             (extend header-value)
             (setf state 2))
            (2 ;; character in header-value
             (extend header-value)))))))))

(defun parse-key-values (key-value-string)
  "Returns an alist of the keys and values in KEY-VALUE-STRING.

KEY-VALUE-STRING is of the form: (\w+=\w+;)*"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (array character (*)) key-value-string))
  (flet ((make-adjustable-string (&optional (default-size 20))
           (make-array default-size
                       :element-type 'character
                       :adjustable t
                       :fill-pointer 0)))
    (let ((key (make-adjustable-string))
          (value (make-adjustable-string))
          (keys-and-values '()))
      (declare (type (array character (*)) key value))
      (loop
         with state = :pre-key
         for char across key-value-string
         do (flet ((extend (string) (vector-push-extend char string))
                   (finish-value ()
                     (setf state :pre-key)
                     (push (cons key value) keys-and-values)
                     (setf key (make-adjustable-string)
                           value (make-adjustable-string))))
              (if (eql :escape state)
                  (extend value)
                  (case char
                    (#\=
                     (ecase state
                       ((:in-double-quote :in-value)
                        (extend value))
                       (:in-key
                        (setf state :in-value))))
                    (#\;
                     (ecase state
                       (:in-double-quote
                        (extend value))
                       ((:in-value :post-value)
                        (finish-value))))
                    (#\"
                     (ecase state
                       (:in-double-quote
                        (setf state :post-value))
                       (:in-value
                        (setf state :in-double-quote))))
                    ((#\Space #\Tab)
                     (ecase state
                       (:in-value
                        (setf state :post-value))
                       ((:pre-key :post-value)
                        nil)
                       (:in-double-quote
                        (extend value))))
                    (#\\
                     (ecase state
                       (:in-double-quote
                        (setf state :escape))))
                    (t
                     (ecase state
                       ((:in-double-quote :in-value)
                        (extend value))
                       (:pre-key
                        (extend key)
                        (setf state :in-key))
                       (:in-key
                        (extend key)))))))
           finally (unless (string= "" key)
                     (push (cons key value) keys-and-values)))
      (nreverse keys-and-values))))

(defun parse-header-value (header-value-string)
  "Returns the value in header-value-string and any associated
  attributes."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (array character (*)) header-value-string))
  (loop
     with value of-type (array character (*)) = (make-array (length header-value-string)
                                                            :element-type 'character
                                                            :adjustable t
                                                            :fill-pointer 0)
     with state = :pre-value
     for offset fixnum upfrom 0
     for char across header-value-string
     do (flet ((extend ()
                 (vector-push-extend char value)))
          (case char
            ((#\Space #\Tab)
             (ecase state
               (:pre-value nil)
               (:post-value nil)))
            (#\;
             ;; done with value.
             (return-from parse-header-value
               (values value (parse-key-values (make-array (- (length header-value-string) 1 offset)
                                                           :element-type 'character
                                                           :displaced-to header-value-string
                                                           :displaced-index-offset (1+ offset))))))
            (t
             (ecase state
               (:pre-value
                (setf state :in-value)
                (extend))
               (:in-value
                (extend))))))
     ;; if we get here then there's a value but no #\; and no attributes.
     finally (return-from parse-header-value
               (values value '()))))

;;;; *** Utility functions

(defun lwsp-char-p (byte)
  "Returns true if BYTE is a linear-whitespace-char (LWSP-char).
Either space or tab, in short."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 8) byte))
  (or (= +Space+ byte)
      (= +Tab+ byte)))

(defun as-ascii-char (byte)
  "Assuming BYTE is an ASCII coded character retun the corresponding character."
  (case byte
    (32 #\Space)
    (9 #\Tab)
    (t (aref (load-time-value
              "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")
             (- byte 33)))))

(defun ascii-string-to-boundry-array (string)
  (map-into (make-array (length string)
                        :element-type '(unsigned-byte 8)
                        :adjustable nil)
            (lambda (char)
              (if (< (char-code char) 128)
                  (char-code char)
                  (error "Bad char for a MIME boundry: ~C" char)))
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
