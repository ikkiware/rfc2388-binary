;; -*- lisp -*-

(in-package :rfc2388-binary)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug* nil
    "When T we compile the code with some logging statements built in.")
  (defmacro debug-message (message &rest message-control)
    (when *debug*
      `(format *debug-io* ,message ,@message-control))))

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

(defgeneric mime-part-p (object)
  (:method ((object mime-part)) t)
  (:method ((object t)) nil))

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
  (:documentation "Returns the mime-header object for the header
  named HEADER-NAME (a string)."))

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
  "Parses MIME entities, returning them as a list.  Each element
in the list is of form: (body headers), where BODY is the
contents of MIME part, and HEADERS are all headers for that part.
BOUNDARY is a string used to separate MIME entities.

This is the convenience interface to READ-MIME, all data is read
into memory and we assume that every byte in the data corresponds
to exactly one character.

The SOURCE and BOUNDARY arguments are passed unchanged to
READ-MIME. See READ-MIME's documentation for details."
  (read-mime source boundary
             (if write-content-to-file
                 (make-mime-file-writer byte-encoder)
                 (make-mime-buffer-writer byte-encoder))))

;;;; ** Implementation

;;;; *** Actual parsers

(defmethod read-mime ((source string) boundary callback-factory)
  (with-input-from-string (source source)
    (read-mime source boundary callback-factory)))

(defmethod read-mime ((source stream) (boundary string) callback-factory)
  (read-mime source (ascii-string-to-boundary-array boundary) callback-factory))

(defmethod read-mime ((source stream) (boundary array) callback-factory)
  (declare (optimize speed))
  (unless (functionp callback-factory)
    (setf callback-factory (fdefinition callback-factory)))
  ;; read up to the first part
  (read-until-next-boundary source boundary #'identity :assume-first-boundary t)
  ;; read headers and boundries until we're done
  (loop
     with keep-on = t
     with parts = '() ;; hold all the parts in this list
     while keep-on
     for part = (make-instance 'mime-part) ;; each iteration around
                                           ;; this loop creates a
                                           ;; part, unless we get a
                                           ;; multipart/mixed
     do (progn
          ;; read in the headers
          (loop named read-headers
            do (multiple-value-bind (found-header name value)
                   (read-next-header source)
                 (if found-header
                     (multiple-value-bind (value attributes)
                         (parse-header-value value)
                       (let ((header (make-instance 'mime-header
                                                    :name name
                                                    :value value
                                                    :attributes attributes)))
                         (push header (headers part))
                         (cond
                           ((string-equal "Content-Type" name)
                            (setf (content-type part) value)
                            (when (get-header-attribute header "charset")
                              (setf (content-charset part) (get-header-attribute header "charset"))))
                           ((string-equal "Content-Length" name)
                            (setf (content-length part) value)))))
                     (progn
                       (setf (headers part) (nreverse (headers part)))
                       (return-from read-headers)))))
          ;; read in the body
          (if (string= "multipart/mixed" (content-type part))
              (progn
                (dolist (nested-part (read-mime source
                                                (get-header-attribute (get-header part "Content-Type") "boundary")
                                                callback-factory))
                  (push nested-part parts))
                (setf keep-on (read-until-next-boundary source boundary
                                                        (lambda (byte)
                                                          (declare (ignore byte))
                                                          (error "Bad data in mime stream."))
                                                        :assume-first-boundary t)))
              (multiple-value-bind (byte-handler finish-callback abort-callback)
                  (funcall callback-factory part)
                (declare (type function byte-handler finish-callback)
                         (type (or null function) abort-callback))
                (let ((ok nil))
                  (unwind-protect
                       (progn
                         (setf keep-on (read-until-next-boundary source boundary byte-handler))
                         (setf ok t))
                    (if ok
                        (push (funcall finish-callback) parts)
                        (when abort-callback
                          (funcall abort-callback))))))))
     finally (return (nreverse parts))))

(defun read-until-next-boundary (stream boundary data-handler &key assume-first-boundary)
  "Reads from STREAM up to the next boundary. For every byte of
data in stream we call DATA-HANDLER passing it the byte. Returns
T if there's more data to be read, NIL otherwise.

The ASSUME-FIRST-BOUNDARY parameter should T if we're reading the
first part of a MIME message, where there is no leading CR LF
sequence."
  ;; Read until  CR|LF|-|-|boundary|-|-|transport-padding|CR|LF
  ;; States:    0  1  2 3 4        5 6 7                    8
  ;; States 6 and 7 are optional
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 8) (*)) boundary)
           (type (function ((unsigned-byte 8)) t) data-handler))
  (let* ((queue (make-array 80 :element-type '(unsigned-byte 8)))
	 (queue-pos 0)
	 (boundary-pos 0)
	 (boundary-length (length boundary))
	 (state (if assume-first-boundary 2 0))
	 (more-data t))
    (declare (type (simple-array (unsigned-byte 8) (80)) queue)
             (type fixnum queue-pos boundary-pos boundary-length state)
             (type boolean more-data))
    (labels ((flush-queue (next-byte)
	       (declare (type (unsigned-byte 8) next-byte))
	       (let ((old-queue-pos queue-pos))
		 (setf boundary-pos 0
		       queue-pos 0
		       more-data t
		       state 0)
		 (funcall data-handler (elt queue 0))
		 (loop :for i :from 1 :below old-queue-pos
		       :do (handle-byte (elt queue i))))
	       (handle-byte next-byte)
               (values))
	     (enqueue-byte (byte)
	       (declare (type (unsigned-byte 8) byte))
	       (setf (elt queue queue-pos) byte)
	       (incf queue-pos)
               (values))
	     (handle-byte (byte)
	       (declare (type (unsigned-byte 8) byte))
	       (ecase state
		 (0 (cond ((= byte #.(char-code #\return))
			   (enqueue-byte byte)
			   (incf state))
			  (t
			   (funcall data-handler byte))))
		 (1 (cond ((= byte #.(char-code #\newline))
			   (enqueue-byte byte)
			   (incf state))
			  (t
			   (flush-queue byte))))
		 ((2 3) (cond ((= byte #.(char-code #\-))
			       (enqueue-byte byte)
			       (incf state))
			      (t
			       (flush-queue byte))))
		 (4 (cond ((= boundary-pos boundary-length)
			   (incf state)
			   (handle-byte byte))
			  ((= byte (elt boundary boundary-pos))
			   (enqueue-byte byte)
			   (incf boundary-pos))
			  (t
			   (flush-queue byte))))
		 (5 (cond ((= byte #.(char-code #\-))
			   (enqueue-byte byte)
			   (incf state))
			  ((linear-whitespace-byte? byte)
			   (enqueue-byte byte)
			   (setf state 7))
			  ((= byte #.(char-code #\return))
			   (enqueue-byte byte)
			   (setf state 8))
			  (t
			   (flush-queue byte))))
		 (6 (cond ((= byte #.(char-code #\-))
			   (enqueue-byte byte)
			   (incf state)
			   (setf more-data nil))
			  (t
			   (flush-queue byte))))
		 (7 (cond ((linear-whitespace-byte? byte)
			   (enqueue-byte byte))
			  ((= byte #.(char-code #\return))
			   (enqueue-byte byte)
			   (incf state))
			  (t
			   (flush-queue byte))))
		 (8 (cond ((= byte #.(char-code #\newline))
			   (return-from read-until-next-boundary more-data))
			  (t
			   (flush-queue byte)))))
               (values)))
      (loop (handle-byte (read-byte stream))))))

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
  #-allegro (declare (optimize (speed 3) (safety 0) (debug 0)))
  #+allegro (declare (optimize (speed 3) (debug 0)))
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
         (debug-message "READ-NEXT-HEADER State: ~S;~%" state)
         (setf byte (read-byte stream))
         (debug-message "                 Byte: ~D (~C) ==> " byte (code-char byte))
         (case byte
           (13 ;; Carriage-Return
            (ecase state
              (0 ;; found a CR. no header
               (setf state 4))
              (2 ;; end of header-value
               (setf state 3))))
           (10 ;; Line-Feed
            (debug-message "Term.~%")
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
               (extend header-value)))))
         (debug-message "~S;~%" state)))))

(defun parse-key-values (key-value-string)
  "Returns an alist of the keys and values in KEY-VALUE-STRING.

KEY-VALUE-STRING is of the form: (\w+=\"\w+\";)*"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string key-value-string))
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
		(t
		 (ecase state
		   ((:in-double-quote :in-value)
		    (extend value))
		   (:pre-key
		    (extend key)
		    (setf state :in-key))
		   (:in-key
		    (extend key))))))
	 finally (unless (string= "" key)
                     (push (cons key value) keys-and-values)))
      (nreverse keys-and-values))))

(defun parse-header-value (header-value-string)
  "Returns the value in header-value-string and any associated
  attributes."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string header-value-string))
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

;;;; *** Support functions for PARSE-MIME

(defun mime-part-headers-to-alist-helper (mime-part content)
  (list content
        (append
         (when (content-length mime-part)
           (list (cons "Content-Length" (content-length mime-part))))
         (when (content-type mime-part)
           (list (cons "Content-Type"
                       (if (content-charset mime-part)
                           (format nil "~A; charset=\"~A\""
                                   (content-type mime-part)
                                   (content-charset mime-part))
                           (content-type mime-part)))))
         (headers mime-part))))

(defun make-mime-file-writer (byte-encoder)
  (lambda (partial-mime-part)
    (let ((temp-filename (make-tmp-file-name)))
      (setf (content partial-mime-part)
            (open temp-filename
                  :direction :output
                  :element-type 'character))
      (values
       (lambda (byte)
         (write-byte (funcall byte-encoder byte)
                     (content partial-mime-part)))
       (lambda (mime-part)
         (mime-part-headers-to-alist-helper
          mime-part
          (open temp-filename
                :direction :input
                :element-type '(unsigned-byte 8))))))))

(defun make-mime-buffer-writer (byte-encoder)
  (lambda (partial-mime-part)
    (setf (content partial-mime-part)
          (make-array (or (content-length partial-mime-part)
                          100)
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0))
    (values
     (lambda (byte)
       (vector-push-extend (funcall byte-encoder byte)
                           (content partial-mime-part)))
     (lambda (mime-part)
       (mime-part-headers-to-alist-helper
        mime-part (content partial-mime-part))))))

(defun make-tmp-file-name ()
  (if (find-package :tbnl)
      (funcall (find-symbol "MAKE-TMP-FILE-NAME" :tbnl))
      (error "WRITE-CONTENT-TO-FILE keyword argument to PARSE-MIME is supported in TBNL only at the moment.")))

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
