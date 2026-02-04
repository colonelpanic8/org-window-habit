;;; org-window-habit.el --- Time window based habits -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: calendar org-mode habit interval window
;; URL: https://github.com/colonelpanic8/org-window-habit
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1") (dash "2.10.0"))

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

;; The `org-habit-window` package extends the capabilities of org-habit to
;; include habits that are not strictly daily. It allows users to define
;; habits that need to be completed a certain number of times within a
;; given time window, for example, 5 times every 7 days.

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
  "Foreground color for indicating required completions for today."
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
  "Number of days before today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-following-days 4
  "Number of days after today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)


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

(defun org-window-habit-entry-p ()
  "Return non-nil if entry at point is an `org-window-habit'.
An entry is considered a window habit if it has either:
- The CONFIG property (unified versioned format), or
- The WINDOW_SPECS property (new format), or
- The WINDOW_DURATION property (simple format)
Property names respect `org-window-habit-property-prefix'."
  (and (org-entry-get nil "TODO")
       (or (org-entry-get nil (org-window-habit-property "CONFIG") t)
           (org-entry-get nil (org-window-habit-property "WINDOW_SPECS") t)
           (org-entry-get nil (org-window-habit-property "WINDOW_DURATION") t))))


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

(define-minor-mode org-window-habit-mode
  "Minor mode that replaces the normal org-habit functionality."
  :lighter nil
  :global t
  :group 'org-window-habit
  :require 'org-window-habit
  (if org-window-habit-mode
      (progn
        (advice-add #'org-habit-parse-todo
                    :around #'org-window-habit-parse-todo-advice)
        (when (fboundp 'org-habit-get-urgency)
          (advice-add #'org-habit-get-urgency
                      :around #'org-window-habit-get-urgency-advice))
        (advice-add #'org-auto-repeat-maybe
                    :around #'org-window-habit-auto-repeat-maybe-advice)
        (advice-add #'org-add-log-note
                    :around #'org-window-habit-add-log-note-advice)
        (advice-add #'org-habit-insert-consistency-graphs
                    :around #'org-window-habit-insert-consistency-graphs-advice))
    (advice-remove #'org-habit-parse-todo #'org-window-habit-parse-todo-advice)
    (when (fboundp 'org-habit-get-urgency)
      (advice-remove #'org-habit-get-urgency #'org-window-habit-get-urgency-advice))
    (advice-remove #'org-auto-repeat-maybe #'org-window-habit-auto-repeat-maybe-advice)
    (advice-remove #'org-add-log-note #'org-window-habit-add-log-note-advice)
    (advice-remove #'org-habit-insert-consistency-graphs #'org-window-habit-insert-consistency-graphs-advice)))

(provide 'org-window-habit)
;;; org-window-habit.el ends here
