;;;; newsynth.lisp 
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

(deftype output-type () '(member :buffer :float :integer))

(defclass stage ()
  ()
  (:documentation "An audio processing stage in a synthesizer."))

(defgeneric output-count (stage)
  (:documentation "Return the number of outputs that a stage provides."))

(defgeneric input-count (stage)
  (:documentation "Return the number of inputs that a stage requires."))

(defgeneric output-types (stage)
  (:documentation "Return a list of output types that the stage provides."))

(defgeneric input-types (stage)
  (:documentation "Return a list of input types that the stage requires."))


(defclass sine-generator (stage)
  ((frequency :initform 440.0 :initarg :frequency :type double-float)
   (buffer-size :initform 256 :initarg :buffer-size :type fixnum)
   (output-buffer :initform nil :type '(simple-array *)))
  (:documentation "Generate a sine wave of a certain frequency."))

(defmethod output-count ((sine-generator stage))
  1)

(defmethod input-count ((sine-generator stage))
  0)

(defmethod output-types ((sine-generator stage))
  '(:buffer))

(defmethod output-types ((sine-generator stage))
  nil)


(defclass synthesizer ()
  ((stages :initform (make-array '(20) :element-type '(or null stage) :adjustable t :fill-pointer 0 :initial-element nil))
   (forward-links :initform nil)
   (backward-links :initform nil)
   (buffer-size :initform 256 :initarg :buffer-size)
   (sample-rate :initform 44100 :initarg :sample-rate :type fixnum)
   (channel-count :initform 2 :initarg :channel-count :type fixnum)
   (pcm-type :initform '(signed-byte 32) :initarg :pcm-type))
  (:documentation "A synthesizer containing a number of stages, a directed graph of connections, and output format information."))

(defun add-stage (synthesizer stage)
  "Add a new stage to the synthesizer."
  (with-slots (stages) synthesizer
    (vector-push-extend stage stages)))

(defun add-connection (synthesizer first-stage second-stage)
  "Add a connection from first-tage to second-stage."
  (with-slots (forward-links backward-links) synthesizer
    (setf forward-links (adjoin (cons first-stage second-stage) forward-links))
    (setf backward-links (adjoin (cons second-stage first-stage) backward-links))))

(defun inputs (synthesizer stage)
  "Find the stages that send input to stage."
  (remove-if-not (lambda (val) (= (car val ) stage)) (slot-value synthesizer 'backward-links)))

(defun outputs (synthesizer stage)
  "Find the stages that receive output from stage."
  (remove-if-not (lambda (val) (= (car val ) stage)) (slot-value synthesizer 'forward-links)))


(defun simulate (synthesizer steps)
  "Simulate a time step."
  (declare (ignorable synthesizer steps))
  nil)


(defun show-pipeline (synthesizer stream)
  (format stream "digraph {~%")
  (with-slots (forward-links) synthesizer
    (dolist (link forward-links)
      (format stream "  ~a -> ~a~%" (car link) (cdr link))))
  (format stream "}~%"))
