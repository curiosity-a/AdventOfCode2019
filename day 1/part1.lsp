(defun read-input (stream)
    (let ((next-value (read stream nil)))
        (if next-value
            (cons next-value (read-input stream))
            nil
        )
    )
)

(defun fuel-requirement (payload)
    (- (floor (/ payload 3)) 2)
)

(print
    (reduce #'+ (mapcar 'fuel-requirement
        (with-open-stream (input-stream (open "input")) (read-input input-stream))
    ))
)
