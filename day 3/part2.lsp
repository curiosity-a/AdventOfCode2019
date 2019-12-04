(defun read-input (stream x y)
    (let (
            (next-direction (read-char stream nil))
            (next-value (read stream nil))
            (delimiter (read-char stream nil))
            (this-point (cons x y))
        )
        (cond
            ((eql #\L next-direction) (setq x (- x next-value)))
            ((eql #\R next-direction) (setq x (+ x next-value)))
            ((eql #\U next-direction) (setq y (- y next-value)))
            ((eql #\D next-direction) (setq y (+ y next-value)))
        )
        (cond
            ((eql #\, delimiter)
                (cons this-point (read-input stream x y))
            )
            (t
                (when delimiter (unread-char delimiter stream))
                (list this-point (cons x y))
            )
        )
    )
)

(defun sort-by-axis (path) ; result (list(hsegments) . list(vsegments)) where each segment is ((x1 . y1) . (x2 . y2))
    (let (
            (point1 (car path))
            (point2 (cadr path))
            (rest (cdr path))
        )
        (when rest
            (let ((sorted (sort-by-axis rest)))
                (if (= (car point1) (car point2))
                    (cons
                        (car sorted)
                        (cons
                            (cons point1 point2)
                            (cdr sorted)
                        )
                    ) ; vertical
                    (cons
                        (cons
                            (cons point1 point2)
                            (car sorted)
                        )
                        (cdr sorted)
                    ) ; horizontal
                )
            )
        )
    )
)

(defun find-all-crossings (hsegments vsegments)
    (remove nil
        (apply #'append
            (mapcar
                (lambda (h)
                    (mapcar
                        (lambda (v)
                            (when (and
                                    (< 0 (* (- (caar h) (caar v)) (- (caar v) (cadr h))))
                                    (< 0 (* (- (cdar v) (cdar h)) (- (cdar h) (cddr v))))
                                ) ; then they intersect in (caar v), (cdar h)
                                (cons (caar v) (cdar h))
                            )
                        )
                        vsegments
                    )
                )
                hsegments
            )
        )
    )
)

(defun get-travel-distance (destination path)
    (when (cdr path)
        (let (
                (point1 (car path))
                (point2 (cadr path))
                (rest (cdr path))
            )
            (cond
                ((and
                        (= (car point1) (car point2) (car destination))
                        (< 0 (* (- (cdr point1) (cdr destination)) (- (cdr destination) (cdr point2))))
                    ) ; destination on vertical segment
                    (abs (- (cdr point1) (cdr destination)))
                )
                ((and
                        (= (cdr point1) (cdr point2) (cdr destination))
                        (< 0 (* (- (car point1) (car destination)) (- (car destination) (car point2))))
                    ) ; destination on horizontal segment
                    (abs (- (car point1) (car destination)))
                )
                (t
                    (+
                        (abs (- (car point1) (car point2)))
                        (abs (- (cdr point1) (cdr point2)))
                        (get-travel-distance destination rest)
                    )
                )
            )
        )
    )
)

(with-open-stream (input-stream (open "input"))
    (let* (
            (path1 (read-input input-stream 0 0))
            (path2 (read-input input-stream 0 0))
            (segments1 (sort-by-axis path1))
            (segments2 (sort-by-axis path2))
        )
        (print
            (apply #'min
                (mapcar
                    (lambda (point) (+
                        (get-travel-distance point path1)
                        (get-travel-distance point path2)
                    ))
                    (append
                        (find-all-crossings (car segments1) (cdr segments2))
                        (find-all-crossings (car segments2) (cdr segments1))
                    )
                )
            )
        )
    )
)
