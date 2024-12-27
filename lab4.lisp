(defun bubble-sort-functional (list &key (key #'identity) (test #'>=))
  (let* ((compare (lambda (a b) (funcall test (funcall key a) (funcall key b)))))
    (labels ((bubble-pass (lst last-index)
               (cond
                 ((or (null lst) (null (cdr lst))) lst)
                 ((<= last-index 0) lst)
                 (t
                  (let ((head (first lst))
                        (tail (cdr lst)))
                    (if (funcall compare head (first tail))
                        (cons (first tail) (bubble-pass (cons head (cdr tail)) (1- last-index)))
                        (cons head (bubble-pass tail (1- last-index))))))))
             (sort-helper (lst last-index)
               (if (or (null lst) (<= last-index 0))
                   lst
                   (sort-helper (bubble-pass lst last-index) (1- last-index)))))
      (sort-helper list (1- (length list))))))
 
(defun propagator-fn (&key (comparator #'>))
  (let ((previous-value nil))
    (lambda (current-value)
      (if (or (null previous-value) (funcall comparator current-value previous-value))
          (setf previous-value current-value)
          previous-value))))

(defun check-result (name function input expected)
  (let ((result (funcall function input)))
    (format t "~:[FAILED~;passed~]... ~a: Expected ~a, got ~a~%"
            (equal result expected) name expected result)))

(defun test-functions ()
  ;; Тести для bubble-sort-functional
  (check-result "Bubble Sort 1"
                (lambda (input) (bubble-sort-functional input))
                '(3 1 4 2)
                '(1 2 3 4))
  (check-result "Bubble Sort 2"
                (lambda (input) (bubble-sort-functional input :key #'abs :test #'<))
                '(-3 -1 4 -2)
                '(4 -3 -2 -1))
  (check-result "Bubble Sort 3"
                (lambda (input) (bubble-sort-functional input))
                '(1 1 4 3)
                '(1 1 3 4))
  (check-result "Bubble Sort 4"
                (lambda (input) (bubble-sort-functional input :key #'abs :test #'<))
                '(1 1 4 3)
                '(4 3 1 1))
  ;; Тести для propagator-fn
  (check-result "Propagator 1"
                (lambda (input) (mapcar (propagator-fn) input))
                '(3 1 4 2)
                '(3 3 4 4))
  (check-result "Propagator 2"
                (lambda (input) (mapcar (propagator-fn) input))
                '(1 2 3)
                '(1 2 3))
  (check-result "Propagator 3"
                (lambda (input) (mapcar (propagator-fn :comparator #'<) input))
                '(1 2 3)
                '(1 1 1)))
(test-functions)

