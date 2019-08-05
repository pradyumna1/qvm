;;;; lazy-entangle.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; This file contains code to efficiently work with qubit subsystems
;;;; (uncorrelated states).

(defstruct (subsystem (:predicate subsystemp))
  "Representation of the state of a subsystem of qubits."
  ;; QUBITS is a bit-set. That is, the Nth bit of QUBITS is 1 if this
  ;; subsystem contains that qubits.
  (qubits 0                            :type unsigned-byte :read-only t)

  ;; STATE is a quantum operator expressed in the standard computation
  ;; basis of QUBITS. For example, if QUBITS is #b101, then the basis
  ;; of STATE is:
  ;;
  ;;     |0>_2 (x) |0>_0
  ;;     |0>_2 (x) |1>_0
  ;;     |1>_2 (x) |0>_0
  ;;     |1>_2 (x) |1>_0
  (state  (make-lisp-cflonum-vector 0) :type quantum-state :read-only t))

(defun make-subsystem-on-qubits (&rest qubits)
  "Make a subsystem (in the zero-state) for the qubits QUBITS.

Note: Duplicate qubits are ignored."
  (declare (dynamic-extent qubits))
  (loop :with qubit-set := 0
        :for q :in qubits
        :do (setf qubit-set (dpb 1 (byte 1 q) qubit-set))
        :finally (return
                   (make-subsystem :qubits qubit-set
                                   :state (let ((state (make-lisp-cflonum-vector
                                                        (expt 2 (logcount qubit-set)))))
                                            (setf (aref state 0) (cflonum 1))
                                            state)))))

(defun subsystem-num-qubits (ss)
  "How many qubits does the subsystem SS represent?"
  (logcount (subsystem-qubits ss)))

(defmethod print-object ((ss subsystem) stream)
  (print-unreadable-object (ss stream :type t :identity nil)
    (let ((qubits (subsystem-qubits ss)))
      (format stream "~Dq" (subsystem-num-qubits ss))
      (when (plusp qubits)
        (format stream ":")
        (loop :for i :below (integer-length (subsystem-qubits ss))
              :when (logbitp i qubits)
                :do (format stream " ~D" i))))))

(defun subsystem-contains-qubit-p (ss qubit)
  "Does the subsystem SS contain the qubits QUBIT?"
  (logbitp qubit (subsystem-qubits ss)))

(defun subsystem-contains-qubits-p (ss qubit-set)
  "Does the subsystem SS contain the qubits designated by the bit-set QUBIT-SET?"
  (zerop (logandc1 (subsystem-qubits ss) qubit-set)))

(defun subsystem-physical-to-logical-qubit (ss phys-qubit)
  "What logical qubit numbner does the physical qubit PHYS-QUBIT have in the subsystem SS?

(The  \"logical qubit\" is the relative position of the qubit in the system.)"
  (and (subsystem-contains-qubit-p ss phys-qubit)
       (logcount (ldb (byte phys-qubit 0) (subsystem-qubits ss)))))

(defun %collapse-integer-by-mask (integer mask)
  "Let INTEGER and MASK be integers of the same bit-length. Remove the bits of INTEGER that correspond to the zeros of MASK."
  (loop :with collapsed := 0
        :with j := 0
        :for i :below (integer-length mask)
        :when (logbitp i mask)
          :do (setf collapsed (dpb (ldb (byte 1 i) integer)
                                   (byte 1 j) collapsed))
              (incf j)
        :finally (return collapsed)))

;;; Maybe a future TODO:
;;;
;;;    - Allow JOIN-SUBSYSTEMS to take any number of args.
;;;
;;;    - Allow EJECT-QUBIT-FROM-SUBSYSTEM to take many qubits.

(defun join-subsystems (ss1 ss2)
  "Join two (disjoint) qubit subsystems SS1 and SS2 into a larger qubit subsystem."
  (let* ((q1 (subsystem-qubits ss1))
         (q2 (subsystem-qubits ss2))
         (new-qubits (logior q1 q2))
         (collapsed-q1 (%collapse-integer-by-mask q1 new-qubits))
         (collapsed-q2 (%collapse-integer-by-mask q2 new-qubits)))
    (unless (zerop (logand q1 q2))
      (error "The subsystems can't be joined because they share qubits."))
    (let* ((new-state-size (expt 2 (logcount new-qubits)))
           (new-state (make-lisp-cflonum-vector new-state-size)))
      (loop :with ss1-state := (subsystem-state ss1)
            :with ss2-state := (subsystem-state ss2)
            :for addr :below new-state-size
            :do (setf (aref new-state addr)
                      (* (aref ss1-state (%collapse-integer-by-mask addr collapsed-q1))
                         (aref ss2-state (%collapse-integer-by-mask addr collapsed-q2)))))
      (make-subsystem :qubits new-qubits
                      :state new-state))))

(defun eject-qubit-from-subsystem (ss qubit)
  "Eject the qubit QUBIT from the subsystem SS."
  (if (not (subsystem-contains-qubit-p ss qubit))
      ss
      (let* ((old-state (subsystem-state ss))
             (new-state (make-lisp-cflonum-vector (/ (length old-state) 2))))
        ;; Populate the state.
        (dotimes (i (length old-state))
          (incf (aref new-state (eject-bit i qubit)) (aref old-state i)))
        ;; Build the new subsystem.
        (make-subsystem :qubits (eject-bit (subsystem-qubits ss) qubit)
                        :state (normalize-wavefunction new-state)))))

