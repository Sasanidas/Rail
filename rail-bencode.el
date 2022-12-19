;;; rail-bencode.el --- Rail-Bencode encoding / decoding -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Modified by: Fermin MF <fmfs@posteo.net>

;;; Commentary:

;; This file is based on https://github.com/skeeto/emacs-bencode


;; This package provides a strict and robust [bencode][bencode]
;; encoder and decoder.Encoding is precise, taking into account
;; character encoding issues.As such, the encoder always returns
;; unibyte data intended to be written out as raw binary data without
;; additional character encoding.When encoding strings and keys,
;; UTF-8 is used by default.The decoder strictly valides its input,
;; rejecting invalid inputs.

;; The API entrypoints are:
;; * `rail-bencode-encode'
;; * `rail-bencode-encode-to-buffer'
;; * `rail-bencode-decode'
;; * `rail-bencode-decode-from-buffer'

;;; Code:

(require 'cl-lib)

(define-error 'rail-bencode "Rail-Bencode error")
(define-error 'rail-bencode-unsupported-type "Type cannot be encoded" 'rail-bencode)
(define-error 'rail-bencode-invalid-key "Not a valid dictionary key" 'rail-bencode)
(define-error 'rail-bencode-invalid-plist "Plist is invalid" 'rail-bencode)
(define-error 'rail-bencode-invalid-byte "Invalid input byte" 'rail-bencode)
(define-error 'rail-bencode-overflow "Integer too large" 'rail-bencode)
(define-error 'rail-bencode-end-of-file "End of file during parsing"
	      '(rail-bencode end-of-file))

(defsubst rail-bencode--int (object)
  "Encode OBJECT as an integer into the current buffer."
  (insert "i" (number-to-string object) "e"))

(defsubst rail-bencode--string (object coding-system)
  "Encode OBJECT as a string into the current buffer."
  (if (multibyte-string-p object)
      (let ((encoded (encode-coding-string object coding-system :nocopy)))
        (insert (number-to-string (length encoded)) ":" encoded))
    (insert (number-to-string (length object)) ":" object)))

(defsubst rail-bencode--hash-table-entries (object coding-system)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((entries ()))
    (maphash (lambda (key value)
               (cond
                ((multibyte-string-p key)
                 (let ((encoded (encode-coding-string
                                 key coding-system :nocopy)))
                   (push (cons encoded value) entries)))
                ((stringp key)
                 (push (cons key value) entries))
                ((signal 'rail-bencode-invalid-key key))))
             object)
    (cl-sort entries #'string< :key #'car)))

(defsubst rail-bencode--plist-entries (object coding-system)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((plist object)
        (entries ()))
    (while plist
      (let ((key (pop plist)))
        (unless (keywordp key)
          (signal 'rail-bencode-invalid-key key))
        (when (null plist)
          (signal 'rail-bencode-invalid-plist object))
        (let ((name (substring (symbol-name key) 1))
              (value (pop plist)))
          (if (multibyte-string-p name)
              (let ((encoded (encode-coding-string
                              name coding-system :nocopy)))
                (push (cons encoded value) entries))
            (push (cons name value) entries)))))
    (cl-sort entries #'string< :key #'car)))

(cl-defun rail-bencode-encode (object &key (coding-system 'utf-8))
  "Return a unibyte string encoding OBJECT with rail-bencode.

:coding-system -- coding system for encoding strings into byte strings (utf-8)

Supported types:
* Integer
* Multibyte and unibyte strings
* List of supported types
* Vector of supproted types (encodes to list)
* Hash table with string keys (encodes to dictionary)
* Plist with keyword symbol keys (encodes to dictionary)

When multibyte strings are encountered either as values or dictionary
keys, they are encoded with the specified coding system (default:
UTF-8). The same coding system must be used when decoding.

Possible error signals:
* rail-bencode-unsupported-type
* rail-bencode-invalid-key
* rail-bencode-invalid-plist

This function is not recursive. It is safe to input very deeply
nested data structures."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (rail-bencode-encode-to-buffer object :coding-system coding-system)
    (buffer-string)))

(cl-defun rail-bencode-encode-to-buffer (object &key (coding-system 'utf-8))
  "Like `rail-bencode-encode' but to the current buffer at point."
  (let ((stack (list (cons :new object))))
    (while stack
      (let* ((next (car stack))
             (value (cdr next)))
        (cl-case (car next)
          ;; Start encoding a new, unexamined value
          (:new
           (pop stack)
           (cond ((integerp value)
                  (rail-bencode--int value))
                 ((stringp value)
                  (rail-bencode--string value coding-system))
                 ((and (consp value)
                       (keywordp (car value)))
                  (insert "d")
                  (let ((entries (rail-bencode--plist-entries value coding-system)))
                    (push (cons :dict entries) stack)))
                 ((listp value)
                  (insert "l")
                  (push (cons :list value) stack))
                 ((vectorp value)
                  (insert "l")
                  (push (cons :vector (cons 0 value)) stack))
                 ((hash-table-p value)
                  (insert "d")
                  (let ((entries (rail-bencode--hash-table-entries
                                  value coding-system)))
                    (push (cons :dict entries) stack)))
                 ((signal 'rail-bencode-unsupported-type object))))
          ;; Continue encoding dictionary
          ;; (:dict . remaining-dict)
          (:dict
           (if (null value)
               (progn
                 (pop stack)
                 (insert "e"))
             (let* ((entry (car value))
                    (key (car entry)))
               (insert (number-to-string (length key)) ":" key)
               (setf (cdr next) (cdr value))
               (push (cons :new (cdr entry)) stack))))
          ;; Continue encoding list
          ;; (:list . remaining-list)
          (:list
           (if (null value)
               (progn
                 (pop stack)
                 (insert "e"))
             (setf (cdr next) (cdr value))
             (push (cons :new (car value)) stack)))
          ;; Continue encoding vector (as list)
          ;; (:vector index . vector)
          (:vector
           (let ((i (car value))
                 (v (cdr value)))
             (if (= i (length v))
                 (progn
                   (pop stack)
                   (insert "e"))
               (setf (car value) (+ i 1))
               (push (cons :new (aref v i)) stack)))))))))

(defsubst rail-bencode--decode-int ()
  "Decode an integer from the current buffer at point."
  (forward-char)
  (let ((start (point)))
    ;; Don't allow leading zeros
    (if (eql (char-after) ?0)
        ;; Unless the value *is* zero
        (prog1 0
          (forward-char)
          (unless (eql (char-after) ?e)
            (signal 'rail-bencode-invalid-byte
                    (cons (char-after) (point))))
          (forward-char))
      ;; Skip minus sign
      (when (eql (char-after) ?-)
        (forward-char)
        ;; Negative zero not allowed
        (when (eql (char-after) ?0)
          (signal 'rail-bencode-invalid-byte
                  (cons (char-after) (point)))))
      ;; Check for empty integer
      (when (eql ?e (char-after))
        (signal 'rail-bencode-invalid-byte
                (cons (char-after) (point))))
      ;; Skip over digits
      (unless (re-search-forward "[^0-9]" nil :noerror)
        (signal 'rail-bencode-end-of-file (point)))
      ;; Check for terminator
      (unless (eql ?e (char-before))
        (signal 'rail-bencode-invalid-byte
                (cons (char-before) (point))))
      ;; Try to parse the digits
      (let* ((string (buffer-substring start (point)))
             (result (string-to-number string)))
        (if (floatp result)
            (signal 'rail-bencode-overflow (cons string result))
          result)))))

(defsubst rail-bencode--decode-string (coding-system)
  "Decode a string from the current buffer at point.

Returns cons of (raw . decoded)."
  (let ((start (point)))
    (if (eql (char-after) ?0)
        ;; Handle zero length as a special case
        (progn
          (forward-char)
          (if (eql (char-after) ?:)
              (prog1 '("" . "")
                (forward-char))
            (signal 'rail-bencode-invalid-byte
                    (cons (char-after) (point)))))
      ;; Skip over length digits
      (unless (re-search-forward "[^0-9]" nil :noerror)
        (signal 'rail-bencode-end-of-file (point)))
      ;; Did we find a colon?
      (unless (eql ?: (char-before))
        (signal 'rail-bencode-invalid-byte
                (cons (char-before) (point))))
      (let* ((length-string (buffer-substring start (- (point) 1)))
             (length (string-to-number length-string)))
        (when (floatp length)
          (signal 'rail-bencode-overflow
                  (cons length-string length)))
        (when (> (+ (point) length) (point-max))
          (signal 'rail-bencode-end-of-file (+ (point) length)))
        (let ((string (buffer-substring (point) (+ (point) length))))
          (prog1 (cons string
                       (decode-coding-string string coding-system :nocopy))
            (forward-char length)))))))

(defsubst rail-bencode--to-plist (list)
  "Convert a series of parsed dictionary entries into a plist."
  (let ((plist ()))
    (while list
      (push (pop list) plist)
      (push (intern (concat ":" (pop list))) plist))
    plist))

(defsubst rail-bencode--to-hash-table (list)
  "Convert a series of parsed dictionary entries into a hash table."
  (let ((table (make-hash-table :test 'equal)))
    (prog1 table
      (while list
        (let ((value (pop list))
              (key (pop list)))
          (setf (gethash key table) value))))))

(cl-defun rail-bencode-decode-from-buffer
    (&key (list-type 'list) (dict-type 'plist) (coding-system 'utf-8))
  "Like `rail-bencode-decode' but from the current buffer starting at point.

The point is left where parsing finished. You may want to reject
inputs with data trailing beyond the point."
  ;; Operations are pushed onto an operation stack. One operation is
  ;; executed once per iteration. Some operations push multiple new
  ;; operations onto the stack. When no more operations are left,
  ;; return the remaining element from the value stack.
  (let ((op-stack '(:read))       ; operations stack
        (value-stack (list nil))  ; stack of parsed values
        (last-key-stack ()))      ; last key seen in top dictionary
    (while op-stack
      (cl-case (car op-stack)
        ;; Figure out what type of value is to be read next and
        ;; prepare stacks accordingly.
        (:read
         (pop op-stack)
         (cl-case (char-after)
           ((nil) (signal 'rail-bencode-end-of-file (point)))
           (?i (push (rail-bencode--decode-int) (car value-stack)))
           (?l (forward-char)
               (push :list op-stack)
               (push nil value-stack))
           (?d (forward-char)
               (push :dict op-stack)
               (push nil value-stack)
               (push nil last-key-stack))
           ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
            (push (cdr (rail-bencode--decode-string coding-system))
                  (car value-stack)))
           (t (signal 'rail-bencode-invalid-byte (point)))))
        ;; Read a key and push it onto the list on top of the value stack
        (:key
         (pop op-stack)
         (let* ((string (rail-bencode--decode-string coding-system))
                (raw (car string))
                (key (cdr string))
                (last-key (car last-key-stack)))
           (when last-key
             (when (string= last-key raw)
               (signal 'rail-bencode-invalid-key (cons 'duplicate key))))
           (setf (car last-key-stack) raw)
           (push key (car value-stack))))
        ;; End list, or queue operations to read another value
        (:list
         (if (eql (char-after) ?e)
             (let ((result (nreverse (pop value-stack))))
               (forward-char)
               (pop op-stack)
               (if (eq list-type 'vector)
                   (push (vconcat result) (car value-stack))
                 (push result (car value-stack))))
           (push :read op-stack)))
        ;; End dict, or queue operations to read another entry
        (:dict
         (if (eql (char-after) ?e)
             (let ((result (pop value-stack)))
               (forward-char)
               (pop op-stack)
               (pop last-key-stack)
               (if (eq dict-type 'hash-table)
                   (push (rail-bencode--to-hash-table result) (car value-stack))
                 (push (rail-bencode--to-plist result) (car value-stack))))
           (push :read op-stack)
           (push :key op-stack)))))
    (caar value-stack)))

(cl-defun rail-bencode-decode
    (string &key (list-type 'list) (dict-type 'plist) (coding-system 'utf-8))
  "Decode rail-bencode data from STRING.

:coding-system -- coding system for decoding byte strings (utf-8)
:dict-type -- target format for dictionaries (symbol: plist, hash-table)
:list-type -- target format for lists (symbol: list, vector)

Input should generally be unibyte. Strings parsed as values and
keys will be decoded using the coding system indicated by the
given coding system (default: UTF-8). The same coding system
should be used as when encoding. There are never decoding errors
since Emacs can preserve arbitrary byte data across encoding and
decoding. See \"Text Representations\" in the Gnu Emacs Lisp
Reference Manual.

Input is strictly validated and invalid inputs are rejected. This
includes dictionary key constraints. Dictionaries are decoded
into plists. Lists are decoded into lists. If an integer is too
large to store in an Emacs integer, the decoder will signal an
overlow error. Signals an error if STRING contains trailing data.

Possible error signals:
* rail-bencode-end-of-file
* rail-bencode-invalid-key
* rail-bencode-invalid-byte
* rail-bencode-overflow

This function is not recursive. It is safe to parse very deeply
nested inputs."
  (let (responses)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(setf responses
	      (append (rail-bencode-decode-from-buffer :list-type list-type
					   :dict-type dict-type
					   :coding-system coding-system)
		      responses)))
      (delete-dups responses))))

(provide 'rail-bencode)

;;; rail-bencode.el ends here
