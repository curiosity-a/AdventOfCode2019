(let ((data nil))
    (with-open-file (input-stream "input")
        (let ((inputs nil) (input nil)) (loop
            (setq input (read-line input-stream nil))
            (when (not input)
                (setq data (make-array (list (length inputs)) :initial-contents inputs))
                (return)
            )
            (map nil
                (lambda (newchar oldchar) (nsubstitute newchar oldchar input))
                "(      "
                "<>xyz=,"
            )
            (push (read-from-string (concatenate 'string input "0 0 0)")) inputs)
        ))
    )
    (loop for count from 0 below 1000 do
        (loop for i from 0 below (length data) do
            (loop for j from 0 below (length data) do
                (loop for k from 0 upto 2
                    for p from 3 upto 5 do
                    (incf (nth p (aref data i)) (signum (- (nth k (aref data j)) (nth k (aref data i)))))
                )
            )
        )
        (loop for i from 0 below (length data) do
            (loop for k from 0 upto 2
                for p from 3 upto 5 do
                (incf (nth k (aref data i)) (nth p (aref data i)))
            )
        )
    )
    (print (reduce #'+ (map 'list
        (lambda (element) (funcall
            (lambda (element) (*
                (+ (first element) (second element) (third element))
                (+ (fourth element) (fifth element) (sixth element))
            ))
            (mapcar #'abs element)
        ))
        data
    )))
)