(defun read-input (stream x y) ; result (list(hsegments) . list(vsegments)) where each segment is ((x1 . y1) . (x2 . y2))
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
                (let ((rest (read-input stream x y)))
                    (if (= x (car this-point))
                        (cons
                            (car rest)
                            (cons
                                (cons this-point (cons x y))
                                (cdr rest)
                            )
                        ) ; vertical
                        (cons
                            (cons
                                (cons this-point (cons x y))
                                (car rest)
                            )
                            (cdr rest)
                        ) ; horizontal
                    )
                )
            )
            (t
                (when delimiter (unread-char delimiter stream))
                (if (= x (car this-point))
                    (cons nil (list (cons this-point (cons x y)))) ; vertical
                    (cons (list (cons this-point (cons x y))) nil) ; horizontal
                )
            )
        )
    )
)

(defun find-closest-crossing-distance (hsegments vsegments)
    (reduce
        (lambda (d1 d2)
            (if (or (not d1) (and d2 (> d1 d2)))
                d2
                d1
            )
        )
        (apply #'append
            (mapcar
                (lambda (h)
                    (mapcar
                        (lambda (v)
                            (when (and
                                    (< 0 (* (- (caar h) (caar v)) (- (caar v) (cadr h))))
                                    (< 0 (* (- (cdar v) (cdar h)) (- (cdar h) (cddr v))))
                                ) ; then they intersect in (caar v), (cdar h)
                                (+ (abs (caar v)) (abs (cdar h)))
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

(with-open-stream (input-stream (open "input"))
    (let* (
            (segments1 (read-input input-stream 0 0))
            (segments2 (read-input input-stream 0 0))
            (closest (find-closest-crossing-distance (car segments1) (cdr segments2)))
            (other-closest (find-closest-crossing-distance (car segments2) (cdr segments1)))
        )
        (print
            (if (or (not closest) (and other-closest (> closest other-closest)))
                other-closest
                closest
            )
        )
    )
)
