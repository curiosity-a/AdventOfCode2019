(defconstant *intcode-debug-output* nil)

(defun argument-mode-to-index-values (arg-mode num-args memory index offset)
    (when (> num-args 0)
        (multiple-value-bind (q r result) (floor arg-mode 10)
            (setq result
                (cond
                    ((= 0 r) (aref memory index))
                    ((= 1 r) index)
                    ((= 2 r) (+ (aref memory index) offset))
                    (t (error "Intcode: Invalid argument mode ~S" r))
                )
            )
            (when (>= result (array-dimension memory 0))
                (adjust-array memory (list (+ 64 result)) :initial-element 0)
            )
            (multiple-value-call #'values
                result
                (argument-mode-to-index-values q (1- num-args) memory (1+ index) offset)
            )
        )
    )
)

(defun execute-intcode (memory input-function output-function &optional continue-information)
    (let (
            (index (or (first continue-information) 0))
            (offset (or (second continue-information) 0))
            (input nil)
            (awaits-input nil)
        )
        (loop (multiple-value-bind (arg-mode opcode) (floor (aref memory index) 100)
            (incf index)
            (cond
                ((= opcode 1) (multiple-value-bind ; add
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A, ~A)~> -> ~A([~A]=~A, [~A]=~A, [~A])"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        (aref memory (+ 2 index))
                        'add
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                        result-index
                    ))
                    (incf index 3)
                    (setf (aref memory result-index)
                        (+ (aref memory arg1-index) (aref memory arg2-index))
                    )
                    (when *intcode-debug-output* (format t " -> ~A" (aref memory result-index)))
                ))
                ((= opcode 2) (multiple-value-bind ; multiply
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A, ~A)~> -> ~A([~A]=~A, [~A]=~A, [~A])"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        (aref memory (+ 2 index))
                        'mul
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                        result-index
                    ))
                    (incf index 3)
                    (setf (aref memory result-index)
                        (* (aref memory arg1-index) (aref memory arg2-index))
                    )
                    (when *intcode-debug-output* (format t " -> ~A" (aref memory result-index)))
                ))
                ((= opcode 3)
                    (setq input (funcall input-function))
                    (when (null input) ; yield when no input
                        (setq awaits-input (list (1- index) offset))
                        (return)
                    )
                    (multiple-value-bind ; read
                        (result-index)
                        (argument-mode-to-index-values arg-mode 1 memory index offset)
                        (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A)~> -> ~A([~A])"
                            #\Newline
                            (1- index)
                            offset
                            (aref memory (1- index))
                            (aref memory index)
                            'read
                            result-index
                        ))
                        (incf index 1)
                        (setf (aref memory result-index) input)
                        (when *intcode-debug-output* (format t " -> ~A" (aref memory result-index)))
                    )
                )
                ((= opcode 4) (multiple-value-bind ; write
                    (arg1-index)
                    (argument-mode-to-index-values arg-mode 1 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A)~> -> ~A([~A]=~A)"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        'write
                        arg1-index
                        (aref memory arg1-index)
                    ))
                    (incf index 1)
                    (funcall output-function (aref memory arg1-index))
                ))
                ((= opcode 5) (multiple-value-bind ; jump if not zero
                    (arg1-index arg2-index)
                    (argument-mode-to-index-values arg-mode 2 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A)~> -> ~A([~A]=~A, [~A]=~A)"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        'jnz
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                    ))
                    (if (= 0 (aref memory arg1-index))
                        (incf index 2)
                        (setq index (aref memory arg2-index))
                    )
                ))
                ((= opcode 6) (multiple-value-bind ; jump if zero
                    (arg1-index arg2-index)
                    (argument-mode-to-index-values arg-mode 2 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A)~> -> ~A([~A]=~A, [~A]=~A)"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        'jz
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                    ))
                    (if (= 0 (aref memory arg1-index))
                        (setq index (aref memory arg2-index))
                        (incf index 2)
                    )
                ))
                ((= opcode 7) (multiple-value-bind ; less than
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A, ~A)~> -> ~A([~A]=~A, [~A]=~A, [~A])"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        (aref memory (+ 2 index))
                        'lt
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                        result-index
                    ))
                    (incf index 3)
                    (setf (aref memory result-index)
                        (if (< (aref memory arg1-index) (aref memory arg2-index)) 1 0)
                    )
                    (when *intcode-debug-output* (format t " -> ~A" (aref memory result-index)))
                ))
                ((= opcode 8) (multiple-value-bind ; equals
                    (arg1-index arg2-index result-index)
                    (argument-mode-to-index-values arg-mode 3 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A, ~A, ~A)~> -> ~A([~A]=~A, [~A]=~A, [~A])"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        (aref memory (1+ index))
                        (aref memory (+ 2 index))
                        'eq
                        arg1-index
                        (aref memory arg1-index)
                        arg2-index
                        (aref memory arg2-index)
                        result-index
                    ))
                    (incf index 3)
                    (setf (aref memory result-index)
                        (if (= (aref memory arg1-index) (aref memory arg2-index)) 1 0)
                    )
                    (when *intcode-debug-output* (format t " -> ~A" (aref memory result-index)))
                ))
                ((= opcode 9) (multiple-value-bind ; offset
                    (arg1-index)
                    (argument-mode-to-index-values arg-mode 1 memory index offset)
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A(~A)~> -> ~A([~A]=~A)"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        (aref memory index)
                        'offs
                        arg1-index
                        (aref memory arg1-index)
                    ))
                    (incf index 1)
                    (incf offset (aref memory arg1-index))
                ))
                ((= opcode 99) ; terminate
                    (when *intcode-debug-output* (format t "~A{~5D,~5D} ~30@<~A~> -> ~A"
                        #\Newline
                        (1- index)
                        offset
                        (aref memory (1- index))
                        'end
                    ))
                    (return)
                )
                (t (error "Intcode: Invalid opcode ~S at ~S" opcode (1- index)))
            )
        ))
        awaits-input
    )
)
; end intcode interpreter

(let (
        (memory nil)
        (x 0)
        (y 0)
        (facing 'up)
        (painted (make-hash-table :test 'equal))
        (phase 'paint)
        (xmin 0)
        (xmax 0)
        (ymin 0)
        (ymax 0)
        (rendered nil)
    )
    (with-open-file (input-stream "input")
        (let ((input (read-from-string
                (concatenate 'string "(" (substitute #\Space #\, (read-line input-stream)) ")")
            )))
            (setq memory (make-array (list (length input)) :initial-contents input :adjustable t))
        )
    )
    (setf (gethash (cons x y) painted) 1)
    (execute-intcode
        memory
        (lambda () (gethash (cons x y) painted 0))
        (lambda (output)
            (cond
                ((eq phase 'paint)
                    (setf (gethash (cons x y) painted) output)
                    (when (< x xmin) (setq xmin x))
                    (when (> x xmax) (setq xmax x))
                    (when (< y ymin) (setq ymin y))
                    (when (> y ymax) (setq ymax y))
                    (setq phase 'turn)
                )
                ((eq phase 'turn)
                    (setq facing (if (= 0 output)
                        ; then turn left
                        (cond
                            ((eq facing 'up) 'left)
                            ((eq facing 'left) 'down)
                            ((eq facing 'down) 'right)
                            ((eq facing 'right) 'up)
                        )
                        ; else turn right
                        (cond
                            ((eq facing 'up) 'right)
                            ((eq facing 'left) 'up)
                            ((eq facing 'down) 'left)
                            ((eq facing 'right) 'down)
                        )
                    ))
                    (cond ; move
                        ((eq facing 'up) (decf y))
                        ((eq facing 'left) (decf x))
                        ((eq facing 'down) (incf y))
                        ((eq facing 'right) (incf x))
                    )
                    (setq phase 'paint)
                )
            )
        )
    )
    (let ((xsize (- xmax xmin -1)) (ysize (- ymax ymin -1)))
        (setq rendered (make-array (list ysize)))
        (loop for i below ysize do (setf (aref rendered i) (make-string xsize :initial-element #\Space)))
        (maphash (lambda (k v) (when (/= 0 v) (setf (aref (aref rendered (cdr k)) (car k)) #\#))) painted)
        (loop for i below ysize do (print (aref rendered i)))
    )
)
