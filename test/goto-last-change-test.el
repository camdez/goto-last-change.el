;;; goto-last-change-test.el --- Tests for goto-last-change.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'goto-last-change)

(defmacro goto-last-change-test-with-buffer (lines &rest body)
  "Run BODY in a temp buffer of LINES numbered lines with undo enabled.
The initial contents are not part of the undo history."
  (declare (indent 1))
  `(with-temp-buffer
     (dotimes (i ,lines)
       (insert (format "line %d\n" (1+ i))))
     (buffer-enable-undo)
     ,@body))

(defun goto-last-change-test-line-pos (line)
  "Return the position at the start of LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun goto-last-change-test-call (&optional repeat)
  "Call `goto-last-change' as a command.
With REPEAT non-nil, call it as if repeating the previous command."
  (let ((this-command 'goto-last-change)
        (last-command (if repeat 'goto-last-change 'ignore)))
    (goto-last-change)))

(ert-deftest goto-last-change-test-insertion ()
  "Point moves to the end of the last insertion."
  (goto-last-change-test-with-buffer 30
    (goto-char (goto-last-change-test-line-pos 15))
    (insert "abc")
    (let ((end (point)))
      (goto-char (point-max))
      (goto-last-change-test-call)
      (should (= (point) end)))))

(ert-deftest goto-last-change-test-deletion ()
  "Point moves to the start of the last deletion."
  (goto-last-change-test-with-buffer 30
    (let ((beg (goto-last-change-test-line-pos 15)))
      (delete-region beg (+ beg 4))
      (goto-char (point-max))
      (goto-last-change-test-call)
      (should (= (point) beg)))))

(ert-deftest goto-last-change-test-repeat ()
  "Repeated calls step back through older changes."
  (goto-last-change-test-with-buffer 50
    (goto-char (goto-last-change-test-line-pos 5))
    (insert "first")
    (let ((first (point)))
      (goto-char (goto-last-change-test-line-pos 40))
      (insert "second")
      (let ((second (point)))
        (goto-char (point-max))
        (goto-last-change-test-call)
        (should (= (point) second))
        (goto-last-change-test-call t)
        (should (= (point) first))))))

(ert-deftest goto-last-change-test-repeat-skips-nearby ()
  "Repeated calls skip changes within `minimal-line-distance' lines."
  (goto-last-change-test-with-buffer 50
    (goto-char (goto-last-change-test-line-pos 5))
    (insert "far")
    (let ((far (point)))
      (goto-char (goto-last-change-test-line-pos 30))
      (insert "near")
      (goto-char (goto-last-change-test-line-pos 33))
      (insert "last")
      (let ((last (point)))
        (goto-char (point-max))
        (goto-last-change-test-call)
        (should (= (point) last))
        (goto-last-change-test-call t)
        (should (= (point) far))))))

(ert-deftest goto-last-change-test-custom-minimal-line-distance ()
  "`goto-last-change-minimal-line-distance' sets the default distance."
  (goto-last-change-test-with-buffer 50
    (goto-char (goto-last-change-test-line-pos 30))
    (insert "near")
    (let ((near (point))
          (goto-last-change-minimal-line-distance 2))
      (goto-char (goto-last-change-test-line-pos 33))
      (insert "last")
      (goto-char (point-max))
      (goto-last-change-test-call)
      (goto-last-change-test-call t)
      (should (= (point) near)))))

(ert-deftest goto-last-change-test-auto-marks-raw-prefix ()
  "A raw prefix arg to `goto-last-change-with-auto-marks' is a distance."
  (goto-last-change-test-with-buffer 50
    (goto-char (goto-last-change-test-line-pos 5))
    (insert "far")
    (goto-char (goto-last-change-test-line-pos 25))
    (insert "near")
    (let ((near (point)))
      (goto-char (goto-last-change-test-line-pos 33))
      (insert "last")
      (goto-char (point-max))
      (let ((this-command 'goto-last-change-with-auto-marks)
            (last-command 'ignore))
        (call-interactively #'goto-last-change-with-auto-marks))
      ;; C-u means a distance of 4, so the change 8 lines away isn't skipped.
      (let ((this-command 'goto-last-change-with-auto-marks)
            (last-command 'goto-last-change-with-auto-marks)
            (current-prefix-arg '(4)))
        (call-interactively #'goto-last-change-with-auto-marks))
      (should (= (point) near)))))

(ert-deftest goto-last-change-test-mark-point ()
  "With MARK-POINT, the mark is set at the starting position."
  (goto-last-change-test-with-buffer 30
    (goto-char (goto-last-change-test-line-pos 15))
    (insert "abc")
    (goto-char (point-max))
    (let ((this-command 'goto-last-change)
          (last-command 'ignore))
      (goto-last-change t))
    (should (= (mark t) (point-max)))))

(ert-deftest goto-last-change-test-no-further-undo ()
  "Repeating past the oldest change signals a `user-error'."
  (goto-last-change-test-with-buffer 30
    (goto-char (goto-last-change-test-line-pos 15))
    (insert "abc")
    (goto-char (point-max))
    (goto-last-change-test-call)
    (should (equal (should-error (goto-last-change-test-call t)
                                 :type 'user-error)
                   '(user-error "No further undo information")))))

(ert-deftest goto-last-change-test-unmodified ()
  "An unmodified buffer signals a `user-error'."
  (goto-last-change-test-with-buffer 30
    (should (equal (should-error (goto-last-change-test-call)
                                 :type 'user-error)
                   '(user-error "Buffer not modified")))))

(ert-deftest goto-last-change-test-undo-disabled ()
  "A buffer with undo disabled signals a `user-error'."
  (with-temp-buffer
    (insert "abc")
    (should (equal (should-error (goto-last-change-test-call)
                                 :type 'user-error)
                   '(user-error "No undo information in this buffer")))))

(ert-deftest goto-last-change-test-apply-entry ()
  "`(apply FUN-NAME . ARGS)' entries carry no position and are skipped."
  (goto-last-change-test-with-buffer 30
    (setq buffer-undo-list (list '(apply ignore) '(5 . 8)))
    (goto-char (point-max))
    (goto-last-change-test-call)
    (should (= (point) 8))))

(ert-deftest goto-last-change-test-apply-delta-entry ()
  "`(apply DELTA BEG END FUN-NAME . ARGS)' entries move point to END."
  (goto-last-change-test-with-buffer 30
    (setq buffer-undo-list (list '(apply 0 10 20 ignore) '(5 . 8)))
    (goto-char (point-max))
    (goto-last-change-test-call)
    (should (= (point) 20))))

(provide 'goto-last-change-test)
;;; goto-last-change-test.el ends here
