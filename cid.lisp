(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-base32))
(require 'ironclad)
(require 'cbor)

(in-package :cl-ipld)

(defun bytes-to-base32-with-no-padding (some-bytes)
  "Like bytes-to-base32, but return base32 string without padding"
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5) )
         (base32-string (make-string word-count)))
    (dotimes (i word-count)
      (setf (aref base32-string i)
            (cl-base32::encode-word (cl-base32::read-word some-bytes i))))
    base32-string))

(defun generate-cid (data)
  (let ((cds (ironclad:digest-sequence
	      :sha256 (cbor:encode data))))
    (concatenate 'string
		 "b"
		 (bytes-to-base32-with-no-padding
		  (concatenate 'vector
			       `#(1 #x71 #x12 ,(length cds))
			       cds)))))
