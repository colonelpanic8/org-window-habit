;;; owh-test-helpers.el --- Shared helpers for org-window-habit tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers for org-window-habit tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)

;;; Test Helpers

(defun owh-test-make-time (year month day &optional hour minute second)
  "Create an Emacs time value for YEAR MONTH DAY HOUR MINUTE SECOND."
  (encode-time (or second 0) (or minute 0) (or hour 0) day month year))

(defun owh-test-times-equal-p (t1 t2)
  "Check if times T1 and T2 are equal, allowing for minor float differences."
  (< (abs (float-time (time-subtract t1 t2))) 1))

;;; Helper for creating test habits with done-times

(defun owh-test-make-habit (window-specs done-times &optional assessment-interval max-reps)
  "Create a habit for testing with WINDOW-SPECS and DONE-TIMES.
ASSESSMENT-INTERVAL defaults to (:days 1).
MAX-REPS defaults to 1."
  (make-instance 'org-window-habit
                 :window-specs window-specs
                 :assessment-interval (or assessment-interval '(:days 1))
                 :max-repetitions-per-interval (or max-reps 1)
                 :done-times (vconcat (sort (copy-sequence done-times)
                                            (lambda (a b) (time-less-p b a))))
                 :start-time nil))

(defun owh-test-day-of-week (time)
  "Get day of week for TIME as a keyword (:sunday, :monday, etc.)."
  (let ((dow (nth 6 (decode-time time))))
    (nth dow '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))))

(provide 'owh-test-helpers)
;;; owh-test-helpers.el ends here
