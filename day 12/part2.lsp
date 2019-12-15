(defun get-period (positions)
    (let ((data nil) (n (length positions)))
        (setq data (make-array (list n 2) :initial-element 0))
        (loop for i below n do (setf (aref data i 0) (elt positions i)))
        (loop with count = 1 do
            (loop for i from 0 below n do
                (loop for j from 0 below n do
                    (incf (aref data i 1) (signum (- (aref data j 0) (aref data i 0))))
                )
            )
            (loop for i from 0 below n do
                (incf (aref data i 0) (aref data i 1))
            )
            (when
                (= n (loop for i from 0 below n
                    while (and
                        (= (aref data i 1) 0)
                        (= (aref data i 0) (elt positions i))
                    )
                    count t
                ))
                (return count)
            )
            (incf count)
        )
    )
)

(let ((inputs nil) (period '#(0 0 0)))
    (with-open-file (input-stream "input")
        (let ((input nil)) (loop
            (setq input (read-line input-stream nil))
            (when (not input)
                (return)
            )
            (map nil
                (lambda (newchar oldchar) (nsubstitute newchar oldchar input))
                "()     "
                "<>xyz=,"
            )
            (push (read-from-string input) inputs)
        ))
    )
    (loop for i from 0 upto 2 do
        (setf (aref period i)
            (get-period (mapcar (lambda (point) (nth i point)) inputs))
        )
    )
    (print (reduce #'lcm period))
)
