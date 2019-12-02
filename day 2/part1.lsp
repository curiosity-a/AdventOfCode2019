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
                    )
                    ((= opcode 2)
                        (setf (aref memory result-index) (* (aref memory arg1-index) (aref memory arg2-index)))
                    )
                )
                (execute-intcode memory (+ 4 index))
            )
        )
    )
)

(print
    (let* (
            (input (with-open-stream (input-stream (open "input")) (read-input input-stream)))
            (data (make-array (list (length input)) :initial-contents input))
        )
        (setf (aref data 1) 12 (aref data 2) 2)
        (execute-intcode data 0)
        (aref data 0)
    )
)
