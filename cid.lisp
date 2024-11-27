(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-base32))
(require 'ironclad)
(require 'cbor)

(in-package :cbor)

(defun encode-alist (value output)
  (declare (type list value)
           (type memstream output)
           #.*optimize*)
  (cond
    ((equal (caar value) "/")
     (let ((seq (cl-ipld::base32-cid-to-binary-cid (cdar value))))
       (write-tag 6 42 output)
       (encode-binary seq output))
     )
    (t
     (with-dictionary (output (length value))
       (loop for (key . val) in (sort (copy-seq value)
				      (lambda (a b)
					(string< (format nil "~a" (car a))
						 (format nil "~a" (car b)))))
             do (%encode key output)
		(%encode val output))))))

(defun encode-list (value output)
  (declare (type list value)
           (type memstream output)
           #.*optimize*)
  (cond
    ((eq 'simple (car value))
     (write-tag 7 (cdr value) output))
    ((and *jsown-semantics*
          (eq :obj (car value)))
     (encode-alist (cdr value) output))
    ((and (every #'consp value)
          ;; (some (lambda (cell)
          ;;         (not (listp (cdr cell))))
          ;;       value)
	  )
     (encode-alist value output))
    (t
     (write-tag 4 (length value) output)
     (loop for val in value do (%encode val output)))))

(in-package :cl-ipld)

(defun bytes-to-base32-with-no-padding (some-bytes)
  "Like bytes-to-base32, but return base32 string without padding"
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5) )
         (base32-string (make-string word-count)))
    (dotimes (i word-count)
      (setf (aref base32-string i)
            (cl-base32::encode-word (cl-base32::read-word some-bytes i))))
    base32-string))

(defun decode-base32-cid (cid)
  (let* ((ret (cl-base32:base32-to-bytes (subseq cid 1)))
	 (len (aref ret 3)))
    (subseq ret 4 (+ len 4))))

(defun base32-cid-to-binary-cid (cid)
  (let ((ret (decode-base32-cid cid)))
    (concatenate '(vector (unsigned-byte 8))
		 #( #x00 ; raw binary
		    #x01 ; CIDv1
		    #x71 ; dag-cbor
		    #x12 ; sha2-256
		   )
		 `#(,(length ret))
		 ret)))

(defun generate-block-cid (block)
  (let ((cds (ironclad:digest-sequence :sha256 block)))
    (concatenate 'string
		 "b"
		 (bytes-to-base32-with-no-padding
		  (concatenate 'vector
			       `#(1 #x71 #x12 ,(length cds))
			       cds)))))

(defun generate-cid (data)
  (generate-block-cid (cbor:encode data)))
