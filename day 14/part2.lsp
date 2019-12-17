(defun get-total-raw (recipes output-material output-amount &optional (spare-materials (make-hash-table)))
    (let (
            (result (make-hash-table))
            (end t)
            (existing-amount (min output-amount (gethash output-material spare-materials 0)))
        )
        (setf (gethash output-material result) output-amount)
        (loop (let ((new-result (make-hash-table)))
            (setq end t)
            (maphash
                (lambda (material amount) (let (existing required ncrafts recipe per-craft extra)
                    (setq
                        existing (min amount (gethash material spare-materials 0))
                        required (- amount existing)
                        recipe (gethash material recipes)
                    )
                    (when (< 0 existing) (decf (gethash material spare-materials) existing))
                    (if (not recipe)
                        (incf (gethash material new-result 0) required)
                        (progn
                            (setq
                                per-craft (cdar recipe)
                                end nil
                            )
                            (multiple-value-setq (ncrafts extra) (ceiling required per-craft))
                            (when (/= 0 extra) (incf (gethash material spare-materials 0) (abs extra)))
                            (when (< 0 ncrafts)
                                (map nil
                                    (lambda (ingredient)
                                        (incf (gethash (car ingredient) new-result 0) (* ncrafts (cdr ingredient)))
                                    )
                                    (cdr recipe)
                                )
                            )
                        )
                    )
                ))
                result
            )
            (setq result new-result)
            (when end (return))
        ))
        (maphash
            (lambda (material amount) (when (= 0 amount) (remhash material spare-materials)))
            spare-materials
        )
        (values result spare-materials)
    )
)

(defun print-material-amounts (material-amounts)
    (if (= 0 (hash-table-count material-amounts))
        (format t "Nothing~%")
        (maphash
            (lambda (material amount) (format t "~A of ~A~%" amount material))
            material-amounts
        )
    )
)

(let (
        ; output-material => ((output-material . output-amount) (input-material . input-amount)*)
        (recipes (make-hash-table))
    )
    (with-open-file (input-stream "input")
        (loop (let ((input (read-line input-stream nil)))
            (when (not input) (return))
            (map nil
                (lambda (newchar oldchar) (nsubstitute newchar oldchar input))
                "   "
                ",=>"
            )
            (let (
                    (input-list (read-from-string (concatenate 'string "(" input ")")))
                    (recipe nil)
                    (material nil)
                    (amount nil)
                )
                (loop
                    (when (null input-list)
                        (setf (gethash (caar recipe) recipes) recipe)
                        (return)
                    )
                    (setq amount (pop input-list) material (pop input-list))
                    (push (cons material amount) recipe)
                )
            )
        ))
    )
    (let (
            (total-ore 1000000000000)
            (ore-cost (gethash 'ore (get-total-raw recipes 'fuel 1)))
            (total-fuel 0)
            fuel-to-make result
        )
        (setq fuel-to-make (floor total-ore ore-cost))
        (loop
            (setq result (get-total-raw recipes 'fuel fuel-to-make))
            (when (> (gethash 'ore result) total-ore) (return))
            (setq total-fuel fuel-to-make)
            (incf fuel-to-make (max 1 (floor (- total-ore (gethash 'ore result)) ore-cost)))
        )
        (format t "~A~%" total-fuel)
    )
)
