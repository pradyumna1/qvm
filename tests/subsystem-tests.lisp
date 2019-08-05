;;;; subsystem-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-subsystem-sundries ()
  (let ((ss0 (qvm::make-subsystem-on-qubits))
        (ss1 (qvm::make-subsystem-on-qubits 0))
        (ss2 (qvm::make-subsystem-on-qubits 0 2))
        (ss3 (qvm::make-subsystem-on-qubits 0 2 4))
        (ss3- (qvm::make-subsystem-on-qubits 0 1 1 2)))
    (is (and (= 0 (qvm::subsystem-num-qubits ss0))
             (= 1 (qvm::subsystem-num-qubits ss1))
             (= 2 (qvm::subsystem-num-qubits ss2))
             (= 3 (qvm::subsystem-num-qubits ss3))
             (= 3 (qvm::subsystem-num-qubits ss3-))))
    (is (= 8 (length (qvm::subsystem-state ss3-))))
    (is (and (qvm::subsystem-contains-qubit-p ss3 0)
             (qvm::subsystem-contains-qubit-p ss3 2)
             (qvm::subsystem-contains-qubit-p ss3 4)))
    (is (not (or (qvm::subsystem-contains-qubit-p ss3 1)
                 (qvm::subsystem-contains-qubit-p ss3 3)
                 (qvm::subsystem-contains-qubit-p ss3 5))))
    (is (and (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss3))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss2))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss1))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss0))))
    (is (not (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss3-))))
    (is (and (= 0 (qvm::subsystem-physical-to-logical-qubit ss3 0))
             (= 1 (qvm::subsystem-physical-to-logical-qubit ss3 2))
             (= 2 (qvm::subsystem-physical-to-logical-qubit ss3 4))))))

(deftest test-join-subsystems ()
  (let ((ss001 (qvm::make-subsystem-on-qubits 0))
        (ss010 (qvm::make-subsystem-on-qubits 1))
        (ss100 (qvm::make-subsystem-on-qubits 2)))
    (is (every #'cflonum=
               (qvm::subsystem-state (qvm::join-subsystems ss001 ss010))
               (qvm::subsystem-state (qvm::join-subsystems ss010 ss100))))
    (is (every #'cflonum=
               (qvm::subsystem-state (qvm::join-subsystems ss010 ss100))
               (qvm::subsystem-state (qvm::join-subsystems ss100 ss001))))
    (is (every #'cflonum=
               (qvm:wf 0 1 0 0)
               (qvm::subsystem-state
                (qvm::join-subsystems (qvm::make-subsystem :qubits #b01
                                                           :state (qvm:wf 0 1))
                                      (qvm::make-subsystem :qubits #b100
                                                           :state (qvm:wf 1 0))))))
    (let ((superposition (qvm:wf (/ (sqrt 2)) (/ (sqrt 2)))))
      (is (every #'cflonum=
                 (qvm:wf 0.5 0.5 0.5 0.5)
                 (qvm::subsystem-state
                  (qvm::join-subsystems (qvm::make-subsystem :qubits #b01
                                                             :state superposition)
                                        (qvm::make-subsystem :qubits #b10
                                                             :state superposition)))))
      (is (every #'cflonum=
                 (apply #'qvm:wf (make-list 16 :initial-element 0.25))
                 (qvm::subsystem-state
                  (qvm::join-subsystems
                   (qvm::join-subsystems (qvm::make-subsystem :qubits #b0001
                                                              :state superposition)
                                         (qvm::make-subsystem :qubits #b0100
                                                              :state superposition))
                   (qvm::join-subsystems (qvm::make-subsystem :qubits #b0010
                                                              :state superposition)
                                         (qvm::make-subsystem :qubits #b1000
                                                              :state superposition)))))))))

(deftest test-eject-qubit-from-subsystem ()
  (let* ((ss010 (qvm::make-subsystem :qubits #b010
                                     :state (qvm:wf (/ (sqrt 2)) (/ (sqrt 2)))))
         (ss101 (qvm::make-subsystem :qubits #b101
                                     :state (qvm:wf 1 0 0 0)))
         (ss111 (qvm::join-subsystems ss010 ss101)))
    (is (every #'cflonum=
               (qvm::subsystem-state ss101)
               (qvm::subsystem-state (qvm::eject-qubit-from-subsystem ss111 1))))))
