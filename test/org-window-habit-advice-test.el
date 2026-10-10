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

(provide 'org-window-habit-advice-test)
;;; org-window-habit-advice-test.el ends here
