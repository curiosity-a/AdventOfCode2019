(with-open-file (input-stream "input")
    (let (
            (layers nil)
            (input (make-string (* 25 6)))
        )
        (loop
            (when (> (* 25 6) (read-sequence input input-stream)) (return))
            (push (cons (count #\0 input) (* (count #\1 input) (count #\2 input))) layers)
        )
        (loop with minimum = (car layers)
            for element in (cdr layers) do
            (when (< (car element) (car minimum)) (setq minimum element))
            finally (print (cdr minimum))
        )
    )
)