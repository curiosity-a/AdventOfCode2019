(defun angle (dx dy)
    (if (= 0 dx)
        (if (< dy 0) pi 0)
        (let ((result nil))
            (setq result (cond
                ((= 0 dy) (/ pi 2))
                (t (atan (/ (- dx) dy)))
            ))
            (when (< result 0) (incf result pi))
            (when (> dx 0) (incf result pi))
            result
        )
    )
)

(let (
        (asteroids nil)
        (area-map nil)
        (station '(25 . 31)) ; station coordinates from the previous part
        (targets nil)
        (count 200)
    )
    (with-open-file (input-stream "input")
        (loop for y upfrom 0 do
            (let ((input (read-line input-stream nil)))
                (when (not input) (return))
                (push input area-map)
                (loop for x from 0 upto (1- (length input)) do
                    (when (char= #\# (aref input x)) (push (cons x y) asteroids))
                )
            )
        )
    )
    (setq area-map (make-array (list (length area-map)) :initial-contents (reverse area-map)))
    (loop
        (setq targets (sort
            (remove nil (mapcar
                (lambda (asteroid)
                    (let* (
                            (dx (- (car station) (car asteroid)))
                            (dy (- (cdr station) (cdr asteroid)))
                            (n (gcd dx dy))
                            (visible t)
                        )
                        (cond
                            ((= 0 dx dy) nil)
                            ((= n 1) (cons (angle dx dy) asteroid))
                            (t
                                (loop for i from 1 upto (1- n) do
                                    (when (char= #\#
                                            (aref (aref area-map
                                                    (+ (cdr asteroid) (* i (/ dy n)))
                                                ) (+ (car asteroid) (* i (/ dx n)))
                                            )
                                        )
                                        (setq visible nil)
                                        (return)
                                    )
                                )
                                (when visible (cons (angle dx dy) asteroid))
                            )
                        )
                    )
                )
                asteroids
            ))
            #'<
            :key #'car
        ))
        (if (>= (length targets) count)
            (let ((found (nth (1- count) targets)))
                (print (+ (* 100 (cadr found)) (cddr found)))
                (return)
            )
            (decf count (length (mapcar
                (lambda (target) (setf (aref (aref area-map (cddr target)) (cadr target)) #\.))
                targets
            )))
        )
    )
)