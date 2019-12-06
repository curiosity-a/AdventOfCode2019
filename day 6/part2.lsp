(defun extract-body (orbit-map body) ; returns remaining-orbit-map, (body . body-orbit-map)
    (when orbit-map (if (string= (caar orbit-map) body)
        (values (cdr orbit-map) (car orbit-map))
        (multiple-value-bind (rest found) (extract-body (cdr orbit-map) body)
            (values (cons (car orbit-map) rest) found)
        )
    ))
)

(defun insert-body-around (orbit-map center body-with-map)
    (cond
        ((null orbit-map) nil)
        ((string= (caar orbit-map) center)
            (cons
                (cons
                    center
                    (cons body-with-map (cdar orbit-map))
                )
                (cdr orbit-map)
            )
        )
        (t (let ((inserted (insert-body-around (cdar orbit-map) center body-with-map)))
            (if inserted
                (cons
                    (cons (caar orbit-map) inserted)
                    (cdr orbit-map)
                )
                (let ((inserted (insert-body-around (cdr orbit-map) center body-with-map)))
                    (when inserted (cons (car orbit-map) inserted))
                )
            )
        ))
    )
)

(defun update-map (orbit-map orbited orbiting)
    (multiple-value-bind (rest existing) (extract-body orbit-map orbiting)
        (when (not existing) (setq existing (cons orbiting nil)))
        (let ((inserted (insert-body-around rest orbited existing)))
            (or inserted (cons (cons orbited (list existing)) rest))
        )
    )
)

(defun read-input (stream) ; returns orbit-map ::= list of (body . orbit-map)
    (let ((result nil))
        (loop (multiple-value-bind (line no-more-lines) (read-line stream)
            (let ((delimiter-index (position #\) line)))
                (setq result (update-map
                    result
                    (subseq line 0 delimiter-index)
                    (subseq line (1+ delimiter-index))
                ))
            )
            (when no-more-lines (return))
        ))
        result
    )
)

(defun pathfind (orbit-map target)
    (loop for (body . body-orbit-map) in orbit-map do
        (if (string= target body)
            (return (list body))
            (let ((path (pathfind body-orbit-map target)))
                (when path (return (cons body path)))
            )
        )
    )
)

(with-open-stream (input-stream (open "input"))
    (let* (
            (orbit-map (read-input input-stream))
            (path-to-you (pathfind orbit-map "YOU"))
            (path-to-santa (pathfind orbit-map "SAN"))
        )
        (print (+
            (length path-to-you)
            (length path-to-santa)
            (* -2 (mismatch path-to-you path-to-santa :test #'string=))
            -2
        ))
    )
)
