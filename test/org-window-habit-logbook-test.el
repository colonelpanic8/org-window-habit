;;; org-window-habit-logbook-test.el --- Logbook tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Logbook tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; ---------------------------------------------------------------------------
;;; Logbook Entry Order Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-fix-logbook-order-after-backdated-completion ()
  "Test that logbook order is fixed after org inserts a backdated entry at top.
When org inserts a log entry at the top (its default behavior), and that entry
has a timestamp older than the existing first entry, our fix should move it
to the correct sorted position."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert "DEADLINE: <2024-01-20 Sat .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      ;; Simulate org inserting a backdated entry at the TOP (the bug)
      ;; This Jan 12 entry is WRONGLY placed before Jan 15
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-12 Fri 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      ;; Verify the initial (wrong) state - Jan 12 is before Jan 15
      (let ((initial-times (save-excursion
                             (org-window-habit-parse-logbook))))
        (should (= (length initial-times) 4))
        ;; First entry is Jan 12 (wrong - should be Jan 15)
        (should (time-equal-p (nth 2 (nth 0 initial-times))
                              (owh-test-make-time 2024 1 12 10 0 0))))

      ;; Now call our fix function
      (goto-char (point-min))
      (org-window-habit-fix-logbook-order)

      ;; Parse the logbook again and verify correct order
      (goto-char (point-min))
      (let ((new-times (save-excursion
                         (org-window-habit-parse-logbook))))
        ;; Should still have 4 entries
        (should (= (length new-times) 4))
        ;; Verify they are now in descending chronological order
        ;; Entry 0: Jan 15 (most recent)
        (should (time-equal-p (nth 2 (nth 0 new-times))
                              (owh-test-make-time 2024 1 15 10 0 0)))
        ;; Entry 1: Jan 12 (moved to correct position)
        (should (time-equal-p (nth 2 (nth 1 new-times))
                              (owh-test-make-time 2024 1 12 10 0 0)))
        ;; Entry 2: Jan 10
        (should (time-equal-p (nth 2 (nth 2 new-times))
                              (owh-test-make-time 2024 1 10 10 0 0)))
        ;; Entry 3: Jan 5 (earliest)
        (should (time-equal-p (nth 2 (nth 3 new-times))
                              (owh-test-make-time 2024 1 5 10 0 0)))))))

(ert-deftest owh-test-fix-logbook-order-already-sorted ()
  "Test that fix does nothing when logbook is already sorted."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-single-entry ()
  "Test that fix handles single entry logbook."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-empty-drawer ()
  "Test that fix handles empty logbook drawer."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-entry-should-go-to-end ()
  "Test fixing when new entry is older than all existing entries."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      ;; Org inserted Jan 1 at top, but it should be at end
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-01 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (org-window-habit-fix-logbook-order)

      (goto-char (point-min))
      (let ((times (save-excursion (org-window-habit-parse-logbook))))
        (should (= (length times) 3))
        ;; Jan 15 should be first
        (should (time-equal-p (nth 2 (nth 0 times))
                              (owh-test-make-time 2024 1 15 10 0 0)))
        ;; Jan 10 should be second
        (should (time-equal-p (nth 2 (nth 1 times))
                              (owh-test-make-time 2024 1 10 10 0 0)))
        ;; Jan 1 should be last (moved from top)
        (should (time-equal-p (nth 2 (nth 2 times))
                              (owh-test-make-time 2024 1 1 10 0 0)))))))

(ert-deftest owh-test-fix-logbook-order-moves-entry-notes ()
  "Test that a moved entry keeps its note lines and others keep theirs."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n"
              ":PROPERTIES:\n"
              ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n"
              ":END:\n"
              ":LOGBOOK:\n"
              "- State \"DONE\"       from \"TODO\"       [2024-01-01 Mon 10:00] \\\\\n"
              "  backdated note\n"
              "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00] \\\\\n"
              "  newest note\n"
              "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n"
              ":END:\n")
      (goto-char (point-min))
      (org-window-habit-fix-logbook-order)
      (should (string-match-p
               (concat ":LOGBOOK:\n"
                       "- State \"DONE\" +from \"TODO\" +\\[2024-01-15 Mon 10:00\\] \\\\\\\\\n"
                       "  newest note\n"
                       "- State \"DONE\" +from \"TODO\" +\\[2024-01-10 Wed 10:00\\]\n"
                       "- State \"DONE\" +from \"TODO\" +\\[2024-01-01 Mon 10:00\\] \\\\\\\\\n"
                       "  backdated note\n"
                       ":END:")
               (buffer-string))))))

(ert-deftest owh-test-parse-logbook-does-not-read-next-entry-logbook ()
  "Test that parse-logbook doesn't read the next entry's LOGBOOK drawer.
When an entry has no state logs, the parser should return nil,
not find and parse a subsequent entry's LOGBOOK. This bug caused
completions from one habit to be incorrectly attributed to the
preceding habit."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      ;; First habit: NO LOGBOOK drawer
      (insert "* TODO Whitening Strips\n")
      (insert "SCHEDULED: <2024-02-02 Mon .+3d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE:    habit\n")
      (insert ":CONFIG:   (:window-specs ((:duration (:days 7) :repetitions 2)))\n")
      (insert ":ID:       first-habit-id\n")
      (insert ":END:\n")
      (insert "\n")
      ;; Second habit: HAS a LOGBOOK drawer with completions
      (insert "* TODO Apartment Cleanliness\n")
      (insert "SCHEDULED: <2024-02-03 Tue .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE:    habit\n")
      (insert ":CONFIG:   (:window-specs ((:duration (:days 7) :repetitions 5)))\n")
      (insert ":ID:       second-habit-id\n")
      (insert ":LAST_REPEAT: [2024-02-02 Mon 19:34]\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-02 Mon 19:34]\n")
      (insert ":END:\n")

      ;; Position at first habit (Whitening Strips)
      (goto-char (point-min))
      (org-back-to-heading t)

      ;; Parse logbook for first habit - should return nil (no logbook)
      (let ((times (save-excursion (org-window-habit-parse-logbook))))
        ;; BUG: Currently returns the second habit's completion
        ;; EXPECTED: Should return nil because first habit has no LOGBOOK
        (should (null times))))))

(ert-deftest owh-test-parse-logbook-reads-inline-state-history ()
  "Test that parse-logbook reads inline state logs in the current entry."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert "SCHEDULED: <2024-02-02 Mon .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: habit\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-02 Mon 08:00]\n")
      (insert "Some note text.\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-03 Tue 09:30]\n")
      (goto-char (point-min))

      (let ((entries (save-excursion (org-window-habit-parse-logbook))))
        (should (= (length entries) 2))
        (should (string= (nth 0 (nth 0 entries)) "DONE"))
        (should (string= (nth 1 (nth 0 entries)) "TODO"))
        (should (time-equal-p (nth 2 (nth 0 entries))
                              (owh-test-make-time 2024 2 2 8 0 0)))
        (should (time-equal-p (nth 2 (nth 1 entries))
                              (owh-test-make-time 2024 2 3 9 30 0)))))))

(ert-deftest owh-test-parse-logbook-reads-mixed-inline-and-drawer-state-history ()
  "Test that parse-logbook reads state logs anywhere in the current entry."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert "SCHEDULED: <2024-02-02 Mon .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: habit\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-02 Mon 08:00]\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-03 Tue 09:30]\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-04 Wed 07:45]\n")
      (goto-char (point-min))

      (let ((entries (save-excursion (org-window-habit-parse-logbook))))
        (should (= (length entries) 3))
        (should (time-equal-p (nth 2 (nth 0 entries))
                              (owh-test-make-time 2024 2 2 8 0 0)))
        (should (time-equal-p (nth 2 (nth 1 entries))
                              (owh-test-make-time 2024 2 3 9 30 0)))
        (should (time-equal-p (nth 2 (nth 2 entries))
                              (owh-test-make-time 2024 2 4 7 45 0)))))))

(ert-deftest owh-test-parse-logbook-does-not-read-next-entry-inline-history ()
  "Test that parse-logbook doesn't read inline logs from the next entry."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO First habit\n")
      (insert "SCHEDULED: <2024-02-02 Mon .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: habit\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 2)))\n")
      (insert ":END:\n\n")
      (insert "* TODO Second habit\n")
      (insert "SCHEDULED: <2024-02-03 Tue .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: habit\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 5)))\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-02-02 Mon 19:34]\n")
      (goto-char (point-min))
      (org-back-to-heading t)

      (should (null (save-excursion (org-window-habit-parse-logbook)))))))

(ert-deftest owh-test-fix-logbook-order-ignores-inline-only-history ()
  "Test that fix-logbook-order leaves entries without drawers unchanged."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-12 Fri 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-create-instance-from-inline-state-history ()
  "Test that instance creation sees inline state logs and sorts done-times."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      ;; Deliberately out of chronological order in the buffer to verify sorting.
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"TODO\"       from \"DONE\"       [2024-01-16 Tue 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (goto-char (point-min))

      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        (should (= (length (oref habit done-times)) 3))
        (should (time-equal-p (aref (oref habit done-times) 0)
                              (owh-test-make-time 2024 1 15 10 0 0)))
        (should (time-equal-p (aref (oref habit done-times) 1)
                              (owh-test-make-time 2024 1 10 10 0 0)))
        (should (time-equal-p (aref (oref habit done-times) 2)
                              (owh-test-make-time 2024 1 5 10 0 0)))))))

(ert-deftest owh-test-create-instance-from-mixed-state-history ()
  "Test that instance creation combines inline and LOGBOOK completions."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 4)))\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-20 Sat 10:00]\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-18 Thu 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert ":END:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-19 Fri 10:00]\n")
      (goto-char (point-min))

      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        (should (= (length (oref habit done-times)) 4))
        (should (time-equal-p (aref (oref habit done-times) 0)
                              (owh-test-make-time 2024 1 20 10 0 0)))
        (should (time-equal-p (aref (oref habit done-times) 1)
                              (owh-test-make-time 2024 1 19 10 0 0)))
        (should (time-equal-p (aref (oref habit done-times) 2)
                              (owh-test-make-time 2024 1 18 10 0 0)))
        (should (time-equal-p (aref (oref habit done-times) 3)
                              (owh-test-make-time 2024 1 15 10 0 0)))))))

(ert-deftest owh-test-parse-completion-times-includes-closing-notes ()
  "Closing notes count as completions; non-done state changes do not."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test habit\n"
            ":LOGBOOK:\n"
            "- CLOSING NOTE [2024-01-15 Mon 10:00] \\\\\n"
            "  note text\n"
            "- State \"TODO\"       from \"WAIT\"       [2024-01-12 Fri 10:00]\n"
            "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n"
            ":END:\n")
    (goto-char (point-min))
    (let ((times (org-window-habit-parse-completion-times)))
      (should (= (length times) 2))
      (should (time-equal-p (nth 0 times) (owh-test-make-time 2024 1 15 10 0 0)))
      (should (time-equal-p (nth 1 times) (owh-test-make-time 2024 1 10 10 0 0))))))

(provide 'org-window-habit-logbook-test)
;;; org-window-habit-logbook-test.el ends here
