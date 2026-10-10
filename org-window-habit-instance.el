;;; org-window-habit-instance.el --- Instance creation from org entries -*- lexical-binding: t; -*-

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

;; Functions for creating org-window-habit instances from org entries.

;;; Code:

(require 'org)
(require 'org-window-habit-config)
(require 'org-window-habit-core)
(require 'org-window-habit-logbook)

;; Forward declarations
(declare-function org-window-habit-entry-get "org-window-habit")


;;; Instance creation from org entry

(defun org-window-habit-create-instance-from-heading-at-point (&optional time)
  "Construct an instance of class `org-window-habit' from the current org entry.
Checks for CONFIG property first (unified format), then falls back to
scattered properties (WINDOW_DURATION, WINDOW_SPECS, etc.) for backwards
compatibility.  TIME defaults to the current time.  Return nil when the
entry's config is inactive at TIME."
  (save-excursion
    (let* ((done-times
            (sort (org-window-habit-parse-completion-times)
                  (lambda (a b) (time-less-p b a))))
           (done-times-vector (vconcat done-times))
           (config-str (org-window-habit-entry-get "CONFIG")))
      (if config-str
          ;; New CONFIG property format
          (org-window-habit-create-instance-from-config
           config-str done-times-vector time)
        ;; Fall back to scattered properties
        (org-window-habit-create-instance-from-scattered-properties
         done-times-vector time)))))

(defun org-window-habit-create-instance-from-config
    (config-str done-times-vector &optional time)
  "Create habit instance from CONFIG-STR with DONE-TIMES-VECTOR.
CONFIG-STR is the value of the CONFIG property (single or versioned config).
TIME defaults to the current time.  Return nil when no config is active then."
  (org-window-habit--create-instance-from-configs
   (org-window-habit-parse-config config-str) done-times-vector time))

(defun org-window-habit-create-instance-from-scattered-properties
    (done-times-vector &optional time)
  "Create habit instance from scattered properties with DONE-TIMES-VECTOR.
This is the backwards-compatible path for habits without CONFIG property;
the properties are converted with `org-window-habit-config-from-properties'.
TIME defaults to the current time.  Return nil when the habit is inactive
then."
  (org-window-habit--create-instance-from-configs
   (org-window-habit-chain-config-dates
    (list (org-window-habit-config-from-properties)))
   done-times-vector time))

(defun org-window-habit--create-instance-from-configs
    (configs done-times-vector &optional time)
  "Create a habit instance from parsed CONFIGS with DONE-TIMES-VECTOR.
TIME defaults to the current time.  Return nil when no config is active then."
  (let ((config (org-window-habit-get-config-for-time
                 configs (or time (current-time)))))
    (when config
      (org-window-habit--make-instance-for-config
       configs config done-times-vector time))))

(provide 'org-window-habit-instance)
;;; org-window-habit-instance.el ends here
