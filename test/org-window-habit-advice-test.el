;;; org-window-habit-advice-test.el --- Org integration and advice tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Org integration and advice tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; Org compatibility tests

(ert-deftest owh-test-org-habit-priority-function-prefers-urgency ()
  "Prefer `org-habit-get-urgency' when both Org APIs are available."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (symbol)
               (memq symbol '(org-habit-get-urgency org-habit-get-priority)))))
    (should (eq (org-window-habit--org-habit-priority-function)
                #'org-habit-get-urgency))))

(ert-deftest owh-test-org-habit-priority-function-falls-back-to-priority ()
  "Fall back to `org-habit-get-priority' on older Org releases."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (symbol)
               (eq symbol 'org-habit-get-priority))))
    (should (eq (org-window-habit--org-habit-priority-function)
                #'org-habit-get-priority))))

(ert-deftest owh-test-org-window-habit-get-urgency-advice-uses-default-priority ()
  "Window habits should bypass Org's list-based urgency calculation."
  (let ((org-window-habit-mode t)
        (org-default-priority 123))
    (should (= (org-window-habit-get-urgency-advice
                (lambda (&rest _) (ert-fail "Advice should not call ORIG"))
                'dummy-habit)
               123))))

(ert-deftest owh-test-org-window-habit-get-urgency-advice-delegates-when-disabled ()
  "When the mode is disabled, the wrapped Org function should run."
  (let ((org-window-habit-mode nil))
    (should (equal (org-window-habit-get-urgency-advice
                    (lambda (&rest args) args)
                    'dummy-habit 'dummy-moment)
                   '(dummy-habit dummy-moment)))))

;;; Completion integration tests

(defun owh-test--complete-weekly-habit (log-done log-repeat)
  "Complete a weekly window habit using LOG-DONE and LOG-REPEAT logging.
Answer any note prompt and return the entry text afterwards."
  (let ((org-log-done log-done)
        (org-log-repeat log-repeat)
        (org-log-into-drawer t)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-window-habit-property-prefix "OWH")
        (was-enabled org-window-habit-mode))
    (org-window-habit-mode 1)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert
           (format (concat "* TODO Weekly\nDEADLINE: <%s .+1d>\n"
                           ":PROPERTIES:\n:STYLE: habit\n"
                           ":OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 1)))\n"
                           ":END:\n:LOGBOOK:\n"
                           "- State \"DONE\"       from \"TODO\"       [%s 09:00]\n"
                           ":END:\n")
                   (format-time-string "%F %a")
                   (format-time-string "%F %a" (time-subtract nil (days-to-time 7)))))
          (goto-char (point-min))
          (org-todo "DONE")
          (run-hooks 'post-command-hook)
          (when-let* ((note-buffer (get-buffer "*Org Note*")))
            (with-current-buffer note-buffer
              (insert "felt good")
              (org-store-log-note)))
          (buffer-string))
      (unless was-enabled
        (org-window-habit-mode -1)))))

(defun owh-test--deadline-in-days-p (entry days)
  "Return non-nil if ENTRY's deadline is DAYS days from today."
  (string-match-p
   (regexp-quote (format "DEADLINE: <%s"
                         (format-time-string
                          "%F" (time-add nil (days-to-time days)))))
   entry))

(ert-deftest owh-test-completion-reschedules-with-time-logging ()
  "Completing with timestamp logging reschedules to the next required date."
  (should (owh-test--deadline-in-days-p
           (owh-test--complete-weekly-habit 'time 'time) 7)))

(ert-deftest owh-test-completion-reschedules-after-repeat-note ()
  "Completing reschedules once a prompted repeat note has been stored."
  (let ((entry (owh-test--complete-weekly-habit 'time 'note)))
    (should (string-match-p "felt good" entry))
    (should (owh-test--deadline-in-days-p entry 7))))

(ert-deftest owh-test-completion-reschedules-with-closing-note ()
  "Completing with closing-note logging records and counts the completion."
  (dolist (log-repeat '(time note))
    (let ((entry (owh-test--complete-weekly-habit 'note log-repeat)))
      (should (string-match-p "CLOSING NOTE" entry))
      (should (owh-test--deadline-in-days-p entry 7)))))

(provide 'org-window-habit-advice-test)
;;; org-window-habit-advice-test.el ends here
