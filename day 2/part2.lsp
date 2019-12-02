(defun read-input (stream)
    (let ((next-value (read stream nil)))
        (if (read-char stream nil)
            (cons next-value (read-input stream))
            nil
        )
    )
)

(defun execute-intcode (memory index)
    (let ((opcode (aref memory index)))
        (if (= opcode 99)
            nil
            (let (
                    (arg1-index (aref memory (1+ index)))
                    (arg2-index (aref memory (+ 2 index)))
                    (result-index (aref memory (+ 3 index)))
                )
                (cond
                    ((= opcode 1)
                        (setf (aref memory result-index) (+ (aref memory arg1-index) (aref memory arg2-index)))
                        (execute-intcode memory (+ 4 index))
                    )
                    ((= opcode 2)
                        (setf (aref memory result-index) (* (aref memory arg1-index) (aref memory arg2-index)))
                        (execute-intcode memory (+ 4 index))
                    )
                )
            )
        )
    )
)

(defun match-output (input target)
    (let ((value1 0) (value2 0))
        (loop
            (let ((data (make-array (list (length input)) :initial-contents input)))
                (setf (aref data 1) value1 (aref data 2) value2)
                (execute-intcode data 0)
                (cond
                    ((= target (aref data 0)) (return (+ (* 100 value1) value2)))
                    ((/= 99 value1) (setq value1 (1+ value1)))
                    ((/= 99 value2) (setq value1 0 value2 (1+ value2)))
                    (t (return nil))
                )
            )
        )
    )
)

(print
    (match-output
        (with-open-stream (input-stream (open "input")) (read-input input-stream))
        19690720
    )
)
