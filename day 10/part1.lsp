(let ((asteroids nil) (area-map nil))
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
    ; also find the station coordinates for the next part
    (print (reduce
        (lambda (a b) (if (> (car a) (car b)) a b))
        (mapcar
            (lambda (this)
                (cons
                    (reduce #'+ (mapcar
                        (lambda (asteroid)
                            (let* (
                                    (dx (- (car this) (car asteroid)))
                                    (dy (- (cdr this) (cdr asteroid)))
                                    (n (gcd dx dy))
                                    (visible 1)
                                )
                                (cond
                                    ((= 0 dx dy) 0)
                                    ((= n 1) 1)
                                    (t
                                        (loop for i from 1 upto (1- n) do
                                            (when (char= #\#
                                                    (aref (aref area-map
                                                            (+ (cdr asteroid) (* i (/ dy n)))
                                                        ) (+ (car asteroid) (* i (/ dx n)))
                                                    )
                                                )
                                                (setq visible 0)
                                                (return)
                                            )
                                        )
                                        visible
                                    )
                                )
                            )
                        )
                        asteroids
                    ))
                    this
                )
            )
            asteroids
        )
    ))
)