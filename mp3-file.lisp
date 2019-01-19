;;;; mp3-file.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :newsynth)

(defclass mp3-output (stage)
  ((file-name :initarg :file-name :type string))
  (:documentation "Write output to an mp3 file."))
(defmethod output-count ((mp3-output stage))
  0)

(defmethod input-count ((mp3-output stage))
  1)

(defmethod output-types ((mp3-output stage))
  '(:buffer))

(defmethod output-types ((mp3-output stage))
  nil)


(defclass mp3-input (stage)
  ((file-name :initarg :file-name :type string))
  (:documentation "Read input from an mp3 file."))
(defmethod output-count ((mp3-input stage))
  1)

(defmethod input-count ((mp3-input stage))
  0)

(defmethod output-types ((mp3-input stage))
  '(:buffer))

(defmethod output-types ((mp3-input stage))
  nil)
