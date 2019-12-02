(defun read-input (stream)
    (let ((next-value (read stream nil)))
        (if next-value
            (cons next-value (read-input stream))
            nil
        )
    )
)

(defun fuel-requirement (payload)
    (let ((fuel (- (floor (/ payload 3)) 2)))
        (if (<= fuel 0)
            0
            (+ fuel (fuel-requirement fuel))
        )
    )
)

(print
    (reduce #'+ (mapcar 'fuel-requirement
        (with-open-stream (input-stream (open "input")) (read-input input-stream))
    ))
)
