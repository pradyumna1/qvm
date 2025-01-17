;;;; benchmark-programs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun bell-program (n)
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     (format t "DECLARE ro BIT[~D]~%" n)
     (format t "H 0~%")
     (loop :for i :below (1- n)
           :do (format t "CNOT ~D ~D~%" i (1+ i)))
     (loop :for i :below n
           :do (format t "MEASURE ~D ro[~D]~%" i i)))))

(defun qft-program (n)
  (qvm-examples:qft-circuit (loop :for i :below n :collect i)))

(defun hadamard-program (n)
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     (format t "DECLARE ro BIT[~D]~%" n)
     (dotimes (i n)
       (format t "H ~D~%" i))
     (dotimes (i n)
       (format t "MEASURE ~D ro[~D]~%" i i)))))

;;; Benchmark from https://github.com/qulacs/qulacs
;;;
;;; Note that the original benchmark as stated requires 100 shots. It
;;; is not clear if the wavefunction can be sampled to do this, or
;;; whether the program must be run 100 times.
(defun qulacs-program (n &key (rx-layers 10))
  "The qulacs benchmark, specified to be 10 layers of random RX rotations interleaved with 9 layers of neighboring CNOTs, followed by measurement of all qubits.

We are assuming the CNOTs are dense on an even number of qubits."
  (assert (plusp rx-layers))
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     ;; Initial RX layer.
     (loop :for q :below n :do
       (format t "RX(~F) ~D~%" (random (* 2 pi)) q))
     ;; CNOT-RX layers.
     (loop :repeat (1- rx-layers) :do
       ;; CNOT's
       (loop :with qubits := (alexandria:shuffle (alexandria:iota n))
             :repeat (floor n 2)
             :do (format t "CNOT ~D ~D~%" (pop qubits) (pop qubits)))
       ;; RX's
       (loop :for q :below n :do
         (format t "RX(~F) ~D~%" (random (* 2 pi)) q)))
     ;; Measurements
     (loop :for q :below n :do
       (format t "MEASURE ~D~%" q)))))

(defun interleaved-measurements-program (n)
  (assert (>= n 2))
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     (dotimes (q n)
       (format t "RX(~F) ~D~%" (random (* 2 pi)) q))
     (dotimes (q n)
       (let ((qubits (subseq (alexandria:shuffle (alexandria:iota n)) 0 2))
             (mqubit (random n)))
         (format t "PSWAP(~F) ~D ~D~%" (random (* 2 pi)) (pop qubits) (pop qubits))
         (format t "MEASURE ~D~%" mqubit))))))

(defun norm-baseline-timing (wf)
  ;; touch all entries
  (qvm::bring-to-zero-state wf)
  (let (timing)
    (with-timing (timing)
      (qvm::%serial-norm wf))
    timing))

(defun perform-benchmark (type num-qubits)
  (check-type num-qubits (integer 1))
  (cond
    ((string-equal type "suite")
     (qvm-benchmarks:run-benchmarks :verbose t))
    ((string-equal type "baseline")
     (let ((q (qvm:make-qvm num-qubits :allocation (funcall **default-allocation** (expt 2 num-qubits)))))
       (format-log "Computing baseline serial norm timing...")
       (finish-output)
       (tg:gc :full t)
       (format-log "Baseline serial norm timing: ~D ms" (norm-baseline-timing (qvm::amplitudes q)))))
    (t
     (let ((p (alexandria:eswitch (type :test #'string-equal)
                ("bell" (bell-program num-qubits))
                ("qft"  (qft-program num-qubits))
                ("hadamard" (hadamard-program num-qubits))
                ("qulacs" (qulacs-program num-qubits))
                ("interleaved-measurements" (interleaved-measurements-program num-qubits))))
           (q (qvm:make-qvm num-qubits
                            :allocation (funcall **default-allocation** (expt 2 num-qubits))))
           timing)
       (qvm:load-program q p :supersede-memory-subsystem t)

       (format-log "Starting ~S benchmark with ~D qubits...~%" type num-qubits)

       (tg:gc :full t)

       (with-timing (timing)
         (time (qvm:run q)))

       (room)
       (terpri)
       (format-log "Total time for program run: ~D ms" timing)))))
