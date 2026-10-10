;;; org-window-habit-advice.el --- Org-habit integration advice -*- lexical-binding: t; -*-

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

;; Advice functions for integrating org-window-habit with org-habit.

;;; Code:

(require 'org)
(require 'org-habit)
(require 'org-window-habit-core)
(require 'org-window-habit-computation)
(require 'org-window-habit-instance)
(require 'org-window-habit-graph)

;; Forward declarations
(defvar org-window-habit-mode)
(defvar org-window-habit-repeat-to-deadline)
(defvar org-window-habit-repeat-to-scheduled)
(declare-function org-window-habit-property "org-window-habit")
(declare-function org-window-habit-entry-p "org-window-habit")


;;; Advice functions

(defun org-window-habit-parse-todo-advice (orig &rest args)
  "Advice for `org-habit-parse-todo' to use window-habit parsing.
ORIG is the original function, ARGS are its arguments."
  (if org-window-habit-mode
      (org-window-habit-create-instance-from-heading-at-point)
    (apply orig args)))

(defun org-window-habit-insert-consistency-graphs-advice (orig &rest args)
  "Advice for `org-habit-insert-consistency-graphs' to use window-habit graphs.
ORIG is the original function, ARGS are its arguments."
  (if org-window-habit-mode
      (org-window-habit-insert-consistency-graphs)
    (apply orig args)))

(defun org-window-habit-get-urgency-advice (orig &rest args)
  "Advice for `org-habit-get-urgency' when window-habit mode is active.
ORIG is the original function, ARGS are its arguments."
  (if org-window-habit-mode
      org-default-priority              ;TODO fix this
    (apply orig args)))


;;; Auto-repeat functionality

(defun org-window-habit-auto-repeat (&optional extra-done-time)
  "Reassign the date of the habit to the next day at which it is required.
EXTRA-DONE-TIME, when non-nil, is treated as an additional completion."
  (interactive)
  (let* ((habit (org-window-habit-create-instance-from-heading-at-point))
         (habit (if (and habit extra-done-time)
                    (org-window-habit--copy-with-done-times
                     habit
                     (vconcat (sort (cons extra-done-time
                                          (append (oref habit done-times) nil))
                                    (lambda (a b) (time-less-p b a)))))
                  habit))
         (required-interval-start
          (when habit
            (org-window-habit-get-next-required-interval habit))))
    (when required-interval-start
      (let ((repeat (org-get-repeat))
            (target-time-string
             (format-time-string (car org-time-stamp-formats)
                                 required-interval-start)))
        (when org-window-habit-repeat-to-deadline
          (org-deadline nil target-time-string)
          (when (null repeat)
            (org-window-habit-add-repeater ".+1d")))
        (when org-window-habit-repeat-to-scheduled
          (org-schedule nil target-time-string)
          (when (null repeat)
            (org-window-habit-add-repeater ".+1d")))))))

(defun org-window-habit-add-repeater (repeater)
  "Add REPEATER string to the last inserted timestamp."
  (save-excursion
	(org-back-to-heading t)
	(when (re-search-forward
		   (concat org-last-inserted-timestamp)
		   (line-end-position 2)
		   t)
	  (goto-char (1- (match-end 0)))
	  (insert-and-inherit " " repeater)
	  (setq org-last-inserted-timestamp
		    (concat (substring org-last-inserted-timestamp 0 -1)
			" " repeater
			(substring org-last-inserted-timestamp -1))))))

(defun org-window-habit-auto-repeat-maybe-advice (orig &rest args)
  "Advice for `org-auto-repeat-maybe'.  Call ORIG with ARGS, then reschedule.
When Org is about to log the completion, rescheduling is left to
`org-window-habit-store-log-note-advice' so that it sees the new entry.
Otherwise the completion is never recorded: reschedule as if it were
and warn."
  (let ((res (apply orig args)))
    (when (and org-window-habit-mode (org-is-habit-p) (not org-log-setup))
      (if (member (car args) org-done-keywords)
          (progn
            (display-warning
             'org-window-habit
             (format "Completion of \"%s\" was not logged and will not be counted; \
set `org-log-repeat' to record habit completions"
                     (org-get-heading t t t t)))
            (org-window-habit-auto-repeat (current-time)))
        (org-window-habit-auto-repeat)))
    res))

(defun org-window-habit-store-log-note-advice (orig &rest args)
  "Advice for `org-store-log-note' that handles `org-window-habit' entries.
ORIG is the original function, ARGS are its arguments.
After a state-change or closing note is stored, this:
1. Fixes logbook order if entries are out of chronological order
2. Triggers the habit rescheduling logic"
  (let ((marker (when (and (memq org-log-note-purpose '(state done))
                           (markerp org-log-note-marker)
                           (marker-buffer org-log-note-marker))
                  (copy-marker org-log-note-marker)))
        (res (apply orig args)))
    (when (and marker org-window-habit-mode)
      (org-with-point-at marker
        (when (org-window-habit-entry-p)
          (org-window-habit-fix-logbook-order))
        (when (org-is-habit-p)
          (org-window-habit-auto-repeat)))
      (set-marker marker nil))
    res))


;;; Reset time management

(defun org-window-habit-set-reset-time (date)
  "Set the reset date for the habit at point to DATE.
When called interactively, prompt for the date using org's date selector.
This causes completions before DATE to be ignored when calculating conformity.
The timestamp is stored as an inactive date-only timestamp."
  (interactive
   (list (org-read-date nil t nil "Reset date: ")))
  (org-entry-put nil (org-window-habit-property "RESET_TIME")
                 (format-time-string (org-time-stamp-format nil t) date)))

(defun org-window-habit-clear-reset-time ()
  "Clear the reset time for the habit at point."
  (interactive)
  (org-entry-delete nil (org-window-habit-property "RESET_TIME")))

(provide 'org-window-habit-advice)
;;; org-window-habit-advice.el ends here
