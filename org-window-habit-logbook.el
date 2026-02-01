;;; org-window-habit-logbook.el --- Logbook parsing and ordering -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Logbook parsing and entry ordering for org-window-habit.

;;; Code:

(require 'cl-lib)
(require 'org)


;;; Logbook drawer bounds

(defun org-window-habit-logbook-drawer-bounds ()
  "Return the bounds (start end) of the LOGBOOK drawer at point, or nil."
  (when (re-search-forward org-logbook-drawer-re nil t)
    (list (match-beginning 0) (match-end 0))))


;;; Logbook entry regexp

(defun org-window-habit-get-logbook-entry-re (&optional state-regexp)
  "Return a regexp matching logbook state change entries.
STATE-REGEXP optionally specifies the pattern for matching state names."
  (unless state-regexp
    (setq state-regexp (rx alphanumeric)))
  (rx
   (: line-start (* space) "-" (* space))
   "State" (* space) (? "\"") (group (* alphanumeric)) (? "\"")
   (* space) (? (: "from" (* space) (? "\"") (group (* alphanumeric)) (? "\"")))
   (* space)
   (regexp org-ts-regexp-inactive)))


;;; Logbook parsing

(defun org-window-habit-parse-logbook ()
  "Parse LOGBOOK drawer at point and return a list of state change entries.
Each entry is (to-state from-state time)."
  (let ((bounds (org-window-habit-logbook-drawer-bounds)))
    (when bounds
      (cl-destructuring-bind (start end) bounds
        (goto-char start)
        (let ((re (org-window-habit-get-logbook-entry-re)))
          (cl-loop while (re-search-forward re end t)
                   collect (list
                            (match-string-no-properties 1)
                            (match-string-no-properties 2)
                            (org-time-string-to-time (match-string-no-properties 3)))))))))


;;; Logbook ordering

(defun org-window-habit-fix-logbook-order ()
  "Fix logbook entry order if the first entry is out of chronological order.
Org-mode inserts new log entries at the top of the LOGBOOK drawer, but when
completing a habit at an earlier date (e.g., via `org-todo-at-date'), this
results in entries being out of order.  This function checks if the first
entry has an older timestamp than the second entry, and if so, moves it to
the correct sorted position.

This is efficient because:
- Most of the time entries are in order, so we just return (O(1) check)
- When out of order, we use binary search to find position (O(log n))
- We only move the one misplaced entry"
  (save-excursion
    (org-back-to-heading t)
    (let ((bounds (org-window-habit-logbook-drawer-bounds)))
      (when bounds
        (cl-destructuring-bind (drawer-start drawer-end) bounds
          (goto-char drawer-start)
          (forward-line 1)
          (let* ((content-start (point))
                 (end-marker (save-excursion
                               (goto-char drawer-end)
                               (line-beginning-position)))
                 (entries (org-window-habit--collect-logbook-entries
                           content-start end-marker)))
            ;; Only proceed if we have at least 2 entries
            (when (>= (length entries) 2)
              (let* ((first-entry (nth 0 entries))
                     (second-entry (nth 1 entries))
                     (first-time (plist-get first-entry :time))
                     (second-time (plist-get second-entry :time)))
                ;; Check if first entry is older than second (out of order)
                (when (time-less-p first-time second-time)
                  (org-window-habit--move-entry-to-sorted-position
                   first-entry (cdr entries) end-marker))))))))))

(defun org-window-habit--collect-logbook-entries (start end)
  "Collect logbook entries between START and END.
Returns a list of plists with :start, :end, :time, and :text properties."
  (save-excursion
    (goto-char start)
    (let ((re (org-window-habit-get-logbook-entry-re))
          entries)
      (while (and (< (point) end)
                  (re-search-forward re end t))
        (let* ((entry-start (match-beginning 0))
               (entry-end (line-beginning-position 2))  ; Start of next line
               (timestamp-str (match-string-no-properties 3))
               (entry-time (org-time-string-to-time timestamp-str))
               (entry-text (buffer-substring entry-start entry-end)))
          (push (list :start entry-start
                      :end entry-end
                      :time entry-time
                      :text entry-text)
                entries)))
      (nreverse entries))))

(defun org-window-habit--move-entry-to-sorted-position (entry other-entries end-marker)
  "Move ENTRY to its correct sorted position among OTHER-ENTRIES.
END-MARKER marks the end of the logbook region.
Uses binary search to find the insertion point efficiently."
  (let* ((entry-time (plist-get entry :time))
         (entry-text (plist-get entry :text))
         (entry-start (plist-get entry :start))
         (entry-end (plist-get entry :end))
         ;; Find insertion point using binary search
         (insert-idx (org-window-habit--binary-search-insert-position
                      other-entries entry-time))
         (insert-pos (if (>= insert-idx (length other-entries))
                         end-marker
                       (plist-get (nth insert-idx other-entries) :start))))
    ;; Delete the misplaced entry first
    (delete-region entry-start entry-end)
    ;; Adjust insert position if it was after the deleted region
    (when (> insert-pos entry-start)
      (setq insert-pos (- insert-pos (- entry-end entry-start))))
    ;; Insert at the correct position
    (goto-char insert-pos)
    (insert entry-text)))

(defun org-window-habit--binary-search-insert-position (entries time)
  "Find index in ENTRIES where an entry with TIME should be inserted.
ENTRIES is a list of entry plists in buffer order (descending time order).
Returns the index where TIME fits to maintain descending order."
  (let ((lo 0)
        (hi (length entries)))
    (while (< lo hi)
      (let* ((mid (/ (+ lo hi) 2))
             (entry (nth mid entries))
             (entry-time (plist-get entry :time)))
        (if (time-less-p time entry-time)
            ;; TIME is older than this entry, search in second half
            (setq lo (1+ mid))
          ;; TIME is newer or equal, search in first half
          (setq hi mid))))
    lo))

(provide 'org-window-habit-logbook)
;;; org-window-habit-logbook.el ends here
