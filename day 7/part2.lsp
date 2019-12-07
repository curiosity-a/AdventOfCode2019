(defun read-input (stream)
    (let ((next-value (read stream nil)))
        (if (read-char stream nil)
            (cons next-value (read-input stream))
            (list next-value)
        )
    )
)

(defun argument-mode-to-index-values (arg-mode num-args memory index)
    (when (> num-args 0)
        (multiple-value-bind (q r) (floor arg-mode 10)
            (multiple-value-call #'values
                (cond
                    ((= 0 r) (aref memory index))
                    ((= 1 r) index)
                    (t (error "Intcode: Invalid argument mode ~S" r))
                )
                (argument-mode-to-index-values q (1- num-args) memory (1+ index))
            )
        )
    )
)

(defun execute-intcode (memory input &optional (starting-index 0))
    (let (
            (index starting-index)
            (this-input nil)
            (next-input input)
            (output nil)
            (awaits-input nil)
        )
        (loop (multiple-value-bind (arg-mode opcode) (floor (aref memory index) 100)
            (incf index)
            (cond
                ((= opcode 1) (multiple-value-bind ; add
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index)
                        (+ (aref memory arg1-index) (aref memory arg2-index))
                    )
                ))
                ((= opcode 2) (multiple-value-bind ; multiply
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index)
                        (* (aref memory arg1-index) (aref memory arg2-index))
                    )
                ))
                ((= opcode 3)
                    (when (null next-input) (setq awaits-input (1- index)) (return)) ;interrupt
                    (multiple-value-bind ; read
                        (result-index)
                        (argument-mode-to-index-values arg-mode 1 memory index)
                        (incf index 1)
                        (setq this-input (car next-input) next-input (cdr next-input))
                        (setf (aref memory result-index) this-input)
                    )
                )
                ((= opcode 4) (multiple-value-bind ; write
                    (arg1-index)
                    (argument-mode-to-index-values arg-mode 1 memory index)
                    (incf index 1)
                    (push (aref memory arg1-index) output)
                ))
                ((= opcode 5) (multiple-value-bind ; jump if not zero
                    (arg1-index arg2-index)
                    (argument-mode-to-index-values arg-mode 2 memory index)
                    (if (= 0 (aref memory arg1-index))
                        (incf index 2)
                        (setq index (aref memory arg2-index))
                    )
                ))
                ((= opcode 6) (multiple-value-bind ; jump if zero
                    (arg1-index arg2-index)
                    (argument-mode-to-index-values arg-mode 2 memory index)
                    (if (= 0 (aref memory arg1-index))
                        (setq index (aref memory arg2-index))
                        (incf index 2)
                    )
                ))
                ((= opcode 7) (multiple-value-bind ; less than
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index)
                        (if (< (aref memory arg1-index) (aref memory arg2-index)) 1 0)
                    )
                ))
                ((= opcode 8) (multiple-value-bind ; equals
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index)
                        (if (= (aref memory arg1-index) (aref memory arg2-index)) 1 0)
                    )
                ))
                ((= opcode 99) ; terminate
                    (return)
                )
                (t (error "Intcode: Invalid opcode ~S at ~S" opcode (1- index)))
            )
        ))
        (values output awaits-input)
    )
)

(defun permutations (source-list)
    (if (or (null source-list) (null (cdr source-list)))
        (list source-list)
        (apply #'append (mapcar
            (lambda (element) (mapcar
                (lambda (permutation) (cons element permutation))
                (permutations (remove element source-list))
            ))
            source-list
        ))
    )
)

(with-open-file (input-stream "input")
    (let* (
            (input (read-input input-stream))
            (possible-settings '(5 6 7 8 9))
            (nprograms (length possible-settings))
            (memory-states (make-array (list nprograms)))
            (continue-from (make-array (list nprograms)))
            (results (make-array (list nprograms)))
        )
        (print (apply #'max (mapcar
            (lambda (phase-settings)
                (loop for i from 0 upto (1- nprograms) do ; init
                    (setf (aref memory-states i) (make-array (list (length input)) :initial-contents input))
                    (multiple-value-bind
                        (result next-index)
                        (execute-intcode (aref memory-states i) (list (nth i phase-settings)))
                        (setf
                            (aref continue-from i) next-index
                            (aref results i) '(0)
                        )
                    )
                )
                (loop
                    (loop for i from 0 upto (1- nprograms) do ; advance programs
                        (multiple-value-bind
                            (result next-index)
                            (execute-intcode
                                (aref memory-states i)
                                (aref results (1- (if (= i 0) nprograms i)))
                                (aref continue-from i)
                            )
                            (setf
                                (aref continue-from i) next-index
                                (aref results i) result
                            )
                        )
                    )
                    (when (position nil continue-from) (return))
                )
                (car (aref results (1- nprograms)))
            )
            (permutations possible-settings)
        )))
    )
)
