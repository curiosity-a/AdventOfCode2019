(defun char-inc (char)
    (character (1+ (char-code char)))
)

(defun make-nondescending (password)
    (let ((n (length password)))
        (loop for i from 0 upto (- n 2) do
            (when (char> (aref password i) (aref password (1+ i)))
                (loop for k from (1+ i) upto (1- n) do
                    (setf (aref password k) (aref password i))
                )
                (return)
            )
        )
    )
)

(defun next-password (password)
    (loop for k from (1- (length password)) downto 0 do
        (if (char= #\9 (aref password k))
            (setf (aref password k) #\0)
            (progn
                (setf (aref password k) (char-inc (aref password k)))
                (return)
            )
        )
    )
    (make-nondescending password)
)

(defun has-duplicate-digit-p (password)
    (loop for i from 0 upto (- (length password) 2) do
        (when (char= (aref password i) (aref password (1+ i)))
            (return t)
        )
    )
)

(with-open-stream (input-stream (open "input"))
    (let* (
            (start (read-line input-stream))
            (end (read-line input-stream))
            (count 0)
        )
        (make-nondescending start)
        (loop until (string>= start end) do
            (when (has-duplicate-digit-p start)
                (incf count)
            )
            (next-password start)
        )
        (print count)
    )
)
