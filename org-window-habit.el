;;; org-window-habit.el --- Time window based habits -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: calendar org-mode habit interval window
;; URL: https://github.com/colonelpanic8/org-window-habit
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1"))

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

;; The `org-window-habit' package extends the capabilities of org-habit to
;; include habits that are not strictly daily. It allows users to define
;; habits that need to be completed a certain number of times within a
;; given time window, for example, 5 times every 7 days.
;;
;; Enable `org-window-habit-mode' and configure habits with the
;; OWH_CONFIG property; see README.org for details.

;;; Code:

(require 'org)
(require 'org-habit)
(require 'org-agenda)


;;; Customization group

(defgroup org-window-habit nil
  "Customization options for the `org-window-habit' package."
  :group 'org-habit)


;;; Property prefix

(defcustom org-window-habit-property-prefix "OWH"
  "Property prefix for org properties used by the `org-window-habit' package."
  :group 'org-window-habit
  :type 'string)


;;; Color customizations

(defcustom org-window-habit-conforming-color "#4d7085"
  "Color to indicate conformity in habit tracking."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-not-conforming-color "#d40d0d"
  "Color to indicate non-conformity in habit tracking."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-required-completion-foreground-color "#000000"
  "Foreground color for indicating required completions."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-non-required-completion-foreground-color "#FFFFFF"
  "Foreground color for indicating non-required completions."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-required-completion-today-foreground-color "#00FF00"
  "Unused; kept for compatibility."
  :group 'org-window-habit
  :type 'string)
(make-obsolete-variable 'org-window-habit-required-completion-today-foreground-color
                        'org-window-habit-graph-foreground-color "0.1.4")

(defcustom org-window-habit-graph-foreground-color "#000000"
  "Foreground color for the current and future intervals of graphs."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-non-conforming-scale 1.0
  "Scale factor for rescaling non-conforming assessment values."
  :group 'org-window-habit
  :type 'float)


;;; Glyph customizations

(defcustom org-window-habit-completion-needed-today-glyph ?☐
  "Glyph character used to show intervals in which a completion is expected."
  :group 'org-window-habit
  :type 'character)

(defcustom org-window-habit-completed-glyph ?✓
  "Glyph character used for the current interval when the habit has been completed."
  :group 'org-window-habit
  :type 'character)


;;; Graph customizations

(defcustom org-window-habit-graph-assessment-fn
  'org-window-habit-default-graph-assessment-fn
  "Function to assess habit graph metrics. It should return color and glyph data."
  :group 'org-window-habit
  :type 'function)

(defcustom org-window-habit-preceding-intervals 21
  "Number of assessment intervals before the current one shown in graphs."
  :group 'org-window-habit
  :type 'integer)

(define-obsolete-variable-alias 'org-window-habit-following-days
  'org-window-habit-following-intervals "0.1.4")

(defcustom org-window-habit-following-intervals 4
  "Number of assessment intervals after the current one shown in graphs."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-show-streak t
  "Whether to append the current conformity streak to consistency graphs.
The streak is the number of consecutive assessment intervals whose
aggregate conforming ratio is at least `org-window-habit-streak-threshold',
ending at the current interval, or at the previous one while the current
interval is not yet conforming."
  :group 'org-window-habit
  :type 'boolean)

(defcustom org-window-habit-streak-format " S%d"
  "Format string used to display a habit's current conformity streak.
The format receives one integer argument: the number of consecutive
conforming assessment intervals."
  :group 'org-window-habit
  :type 'string)

(defcustom org-window-habit-streak-threshold 1.0
  "Minimum aggregate conforming ratio that counts toward a streak."
  :group 'org-window-habit
  :type 'float)


;;; Repeat customizations

(defcustom org-window-habit-repeat-to-deadline t
  "Reassign the deadline of habits on repeat."
  :group 'org-window-habit
  :type 'boolean)

(defcustom org-window-habit-repeat-to-scheduled nil
  "Reassign the scheduled field of habits on repeat."
  :group 'org-window-habit
  :type 'boolean)


;;; Utility functions

(defun org-window-habit-property (name)
  "Return the full property name for NAME with the configured prefix.
For example, if prefix is \"OWH\" and NAME is \"WINDOW_DURATION\",
returns \"OWH_WINDOW_DURATION\"."
  (if org-window-habit-property-prefix
      (format "%s_%s" org-window-habit-property-prefix name)
    name))

(defun org-window-habit-entry-get (name)
  "Return the value of habit property NAME for the entry at point.
NAME is given without `org-window-habit-property-prefix'.  Values are
inherited from parent entries only as `org-use-property-inheritance'
allows."
  (org-entry-get nil (org-window-habit-property name) 'selective))

(defun org-window-habit-entry-p ()
  "Return non-nil if entry at point is an `org-window-habit'.
An entry is considered a window habit if it has either:
- The CONFIG property (unified versioned format), or
- The WINDOW_SPECS property (new format), or
- The WINDOW_DURATION property (simple format)
Property names respect `org-window-habit-property-prefix'.  Properties
are read with `org-window-habit-entry-get'."
  (and (org-entry-get nil "TODO")
       (or (org-window-habit-entry-get "CONFIG")
           (org-window-habit-entry-get "WINDOW_SPECS")
           (org-window-habit-entry-get "WINDOW_DURATION"))))


;;; Load submodules

(require 'org-window-habit-time)
(require 'org-window-habit-config)
(require 'org-window-habit-logbook)
(require 'org-window-habit-core)
(require 'org-window-habit-computation)
(require 'org-window-habit-instance)
(require 'org-window-habit-graph)
(require 'org-window-habit-advice)
(require 'org-window-habit-meta)


;;; Minor mode

(defun org-window-habit--org-habit-priority-function ()
  "Return Org's habit urgency function for the current Org version.
Older Org releases expose `org-habit-get-priority' while newer ones use
`org-habit-get-urgency'."
  (cond
   ((fboundp 'org-habit-get-urgency) #'org-habit-get-urgency)
   ((fboundp 'org-habit-get-priority) #'org-habit-get-priority)))

(define-minor-mode org-window-habit-mode
  "Minor mode that replaces the normal org-habit functionality."
  :lighter nil
  :global t
  :group 'org-window-habit
  :require 'org-window-habit
  (if org-window-habit-mode
      (let ((priority-function
             (org-window-habit--org-habit-priority-function)))
        (advice-add #'org-habit-parse-todo
                    :around #'org-window-habit-parse-todo-advice)
        (when priority-function
          (advice-add priority-function
                      :around #'org-window-habit-get-urgency-advice))
        (advice-add #'org-auto-repeat-maybe
                    :around #'org-window-habit-auto-repeat-maybe-advice)
        (advice-add #'org-store-log-note
                    :around #'org-window-habit-store-log-note-advice)
        (advice-add #'org-habit-insert-consistency-graphs
                    :around #'org-window-habit-insert-consistency-graphs-advice))
    (let ((priority-function
           (org-window-habit--org-habit-priority-function)))
      (advice-remove #'org-habit-parse-todo #'org-window-habit-parse-todo-advice)
      (when priority-function
        (advice-remove priority-function #'org-window-habit-get-urgency-advice))
      (advice-remove #'org-auto-repeat-maybe #'org-window-habit-auto-repeat-maybe-advice)
      (advice-remove #'org-store-log-note #'org-window-habit-store-log-note-advice)
      (advice-remove #'org-habit-insert-consistency-graphs #'org-window-habit-insert-consistency-graphs-advice))))

(provide 'org-window-habit)
;;; org-window-habit.el ends here
