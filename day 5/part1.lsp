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

(defun execute-intcode (memory input)
    (let (
            (index 0)
            (this-input nil)
            (next-input input)
        )
        (loop (multiple-value-bind (arg-mode opcode) (floor (aref memory index) 100)
            (incf index)
            (cond
                ((= opcode 1) (multiple-value-bind ; add
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index) (+ (aref memory arg1-index) (aref memory arg2-index)))
                ))
                ((= opcode 2) (multiple-value-bind ; multiply
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index)
                    (incf index 3)
                    (setf (aref memory result-index) (* (aref memory arg1-index) (aref memory arg2-index)))
                ))
                ((= opcode 3) (multiple-value-bind ; read
                    (result-index)
                    (argument-mode-to-index-values arg-mode 1 memory index)
                    (incf index 1)
                    (setq this-input (car next-input) next-input (cdr next-input))
                    (setf (aref memory result-index) this-input)
                ))
                ((= opcode 4) (multiple-value-bind ; write
                    (arg1-index)
                    (argument-mode-to-index-values arg-mode 1 memory index)
                    (incf index 1)
                    (print (aref memory arg1-index))
                ))
                ((= opcode 99) ; terminate
                    (return)
                )
                (t (error "Intcode: Invalid opcode ~S at ~S" opcode (1- index)))
            )
        ))
    )
)

(with-open-stream (input-stream (open "input"))
    (let ((input (read-input input-stream)))
        (execute-intcode (make-array (list (length input)) :initial-contents input) '(1))
    )
)
