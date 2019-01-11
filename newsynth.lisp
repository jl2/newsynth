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

(defclass stage ()
  ((inputs :initform nil :initarg :inputs)
   (outputs :initform nil :initarg :outputs)
   (previous-value :initform 0.0))
  (:documentation "A processing stage."))

(defclass synthesizer ()
  ((stages :initform (make-array '(20) :element-type '(or null stage) :adjustable t :fill-pointer 0 :initial-element nil))
   (forward-links :initform nil)
   (backward-links :initform nil))
  (:documentation "A synthesizer containing a number of stages and a directed graph of connections."))

(defun add-stage (synthesizer stage)
  "Add a new stage to the synthesizer."
  (with-slots (stages) synthesizer
    (vector-push-extend stage stages)))

(defun add-connection (synthesizer first-stage second-stage)
  "Add a connection from first-tage to second-stage."
  (with-slots (forward-links backward-links) synthesizer
    (adjoin (cons first-stage second-stage) forward-links)
    (adjoin (cons second-stage first-stage) backward-links)))

(defun inputs (synthesizer stage)
  "Find the stages that send input to stage."
  (remove-if-not (lambda (val) (= (car val ) stage)) (slot-value synthesizer 'backward-links)))

(defun outputs (synthesizer stage)
  "Find the stages that receive output from stage."
  (remove-if-not (lambda (val) (= (car val ) stage)) (slot-value synthesizer 'forward-links)))

(defun simulate (synthesizer steps)
  "Simulate a time step."
  nil)

(defun show-pipeline (synthesizer stream)
  (format stream "digraph {~%")
  (with-slots (forward-links) synthesizer
    (dolist (link forward-links)
      (format stream "  ~a -> ~a~%" (car link) (cdr link))))
  (format stream "}~%"))

;; (defclass wave-generator (stage)
;;   ((frequency :initarg :frequency :initform 440))
;;   (:documentation "Generate  wave form."))

;; (defun create-wave-generator (frequency)
;;   (make-instance 'wave-generator :frequency frequency))

;; (defgeneric combine-inputs (stage)
;;   (:documentation "Combine the inputs for a stage."))

;; (defgeneric combine-outputs (stage)
;;   (:documentation "Combine the outputs for a stage."))

;; (defgeneric handle-step (stage)
;;   (
;;   ))

