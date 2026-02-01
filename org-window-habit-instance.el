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

(require 'eieio)
(require 'cl-lib)
(require 'org)
(require 'org-window-habit-time)
(require 'org-window-habit-config)
(require 'org-window-habit-logbook)

;; Forward declarations
(declare-function org-window-habit-property "org-window-habit")

;; Forward declarations for classes
(defvar org-window-habit)
(defvar org-window-habit-window-spec)


;;; Instance creation from org entry

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an instance of class `org-window-habit' from the current org entry.
Checks for CONFIG property first (unified format), then falls back to
scattered properties (WINDOW_DURATION, WINDOW_SPECS, etc.) for backwards
compatibility."
  (save-excursion
    (let* ((done-times
            (cl-loop for state-change-info in (org-window-habit-parse-logbook)
                     if (member (nth 0 state-change-info) org-done-keywords)
                     collect (nth 2 state-change-info)))
           (done-times-vector (vconcat done-times))
           (config-str (org-entry-get nil (org-window-habit-property "CONFIG") t)))
      (if config-str
          ;; New CONFIG property format
          (org-window-habit-create-instance-from-config config-str done-times-vector)
        ;; Fall back to scattered properties
        (org-window-habit-create-instance-from-scattered-properties done-times-vector)))))

(defun org-window-habit-create-instance-from-config (config-str done-times-vector)
  "Create habit instance from CONFIG-STR with DONE-TIMES-VECTOR.
CONFIG-STR is the value of the CONFIG property (single or versioned config)."
  (let* ((configs (org-window-habit-parse-config config-str))
         ;; Get current config (first in list) for active parameters
         (current-config (car configs))
         ;; Extract parameters from current config
         (window-specs-data (plist-get current-config :window-specs))
         (window-specs (cl-loop for args in window-specs-data
                                collect (apply #'make-instance
                                               'org-window-habit-window-spec args)))
         (assessment-interval (or (plist-get current-config :assessment-interval)
                                  '(:days 1)))
         (reschedule-interval (plist-get current-config :reschedule-interval))
         (max-reps (or (plist-get current-config :max-reps-per-interval) 1))
         (only-days (plist-get current-config :only-days))
         ;; :from on single config acts like reset-time
         (reset-time (plist-get current-config :from)))
    (make-instance 'org-window-habit
                   :start-time nil
                   :reset-time reset-time
                   :only-days only-days
                   :window-specs window-specs
                   :assessment-interval assessment-interval
                   :reschedule-interval reschedule-interval
                   :done-times done-times-vector
                   :max-repetitions-per-interval max-reps
                   :configs configs)))

(defun org-window-habit-create-instance-from-scattered-properties (done-times-vector)
  "Create habit instance from scattered properties with DONE-TIMES-VECTOR.
This is the backwards-compatible path for habits without CONFIG property.
Also builds and stores a config plist in the configs slot for uniformity."
  (let* ((assessment-interval-str
          (org-entry-get nil (org-window-habit-property "ASSESSMENT_INTERVAL")))
         (assessment-interval
          (org-window-habit-string-duration-to-plist
           assessment-interval-str :default '(:days 1)))
         (reschedule-interval-str
          (org-entry-get nil (org-window-habit-property "RESCHEDULE_INTERVAL")))
         (reschedule-interval
          (org-window-habit-string-duration-to-plist reschedule-interval-str))
         (max-reps-str
          (org-entry-get nil (org-window-habit-property "MAX_REPETITIONS_PER_INTERVAL") t))
         (max-repetitions-per-interval
          (string-to-number (or max-reps-str "1")))
         (reset-time-str
          (org-entry-get nil (org-window-habit-property "RESET_TIME")))
         (reset-time
          (when reset-time-str
            (org-time-string-to-time reset-time-str)))
         (only-days-str
          (org-entry-get nil (org-window-habit-property "ONLY_DAYS")))
         (only-days
          (org-window-habit-parse-only-days only-days-str))
         (window-specs-objects
          (or (org-window-habit-create-specs)
              (org-window-habit-create-specs-from-perfect-okay)))
         ;; Build window-specs as plists for the config
         (window-specs-plists
          (org-window-habit-build-window-specs-plists-from-properties))
         ;; Build the config plist - only include non-default values
         (config
          (let ((c (list :window-specs window-specs-plists)))
            ;; Only add :assessment-interval if explicitly set
            (when assessment-interval-str
              (setq c (plist-put c :assessment-interval
                                 (org-window-habit-string-duration-to-plist
                                  assessment-interval-str))))
            ;; Only add :reschedule-interval if explicitly set
            (when reschedule-interval-str
              (setq c (plist-put c :reschedule-interval reschedule-interval)))
            ;; Only add :max-reps-per-interval if explicitly set (and not "1")
            (when (and max-reps-str (not (string= max-reps-str "1")))
              (setq c (plist-put c :max-reps-per-interval max-repetitions-per-interval)))
            ;; Only add :only-days if set
            (when only-days
              (setq c (plist-put c :only-days only-days)))
            ;; Convert RESET_TIME to :from
            (when reset-time
              (setq c (plist-put c :from reset-time)))
            c)))
    (make-instance 'org-window-habit
                   :start-time nil
                   :reset-time reset-time
                   :only-days only-days
                   :window-specs window-specs-objects
                   :assessment-interval assessment-interval
                   :reschedule-interval reschedule-interval
                   :done-times done-times-vector
                   :max-repetitions-per-interval max-repetitions-per-interval
                   :configs (list config))))

(defun org-window-habit-build-window-specs-plists-from-properties ()
  "Build window-specs as plists from current heading's scattered properties.
Returns a list of plists suitable for storing in a config."
  (let ((spec-text (org-entry-get nil (org-window-habit-property "WINDOW_SPECS") t)))
    (if spec-text
        ;; WINDOW_SPECS is already in plist format
        (car (read-from-string spec-text))
      ;; Build from WINDOW_DURATION and REPETITIONS_REQUIRED
      (let ((window-length
             (org-window-habit-string-duration-to-plist
              (org-entry-get nil (org-window-habit-property "WINDOW_DURATION")
                             "1d") :default '(:days 1)))
            (repetitions-required
             (string-to-number
              (or (org-entry-get nil
                                 (org-window-habit-property "REPETITIONS_REQUIRED")
                                 t) "1"))))
        (list (list :duration window-length
                    :repetitions repetitions-required))))))

(defun org-window-habit-create-specs ()
  "Parse WINDOW_SPECS property into a list of window-spec objects.
Returns nil if the property is not set."
  (let ((spec-text (org-entry-get nil (org-window-habit-property "WINDOW_SPECS") t)))
    (when spec-text
      (cl-loop for args in (car (read-from-string spec-text))
               collect (apply #'make-instance 'org-window-habit-window-spec args)))))

(defun org-window-habit-create-specs-from-perfect-okay ()
  "Create a single window-spec from WINDOW_DURATION and REPETITIONS_REQUIRED.
This is the simple configuration format for habits with one evaluation window."
  (let*
      ((window-length
        (org-window-habit-string-duration-to-plist
         (org-entry-get nil (org-window-habit-property "WINDOW_DURATION")
                        "1d") :default '(:days 1)))
       (repetitions-required
        (string-to-number
         (or (org-entry-get nil
                            (org-window-habit-property "REPETITIONS_REQUIRED")
                            t) "1"))))
    (list
     (make-instance
      'org-window-habit-window-spec
      :duration window-length
      :repetitions repetitions-required
      ;; Default conforming-value of 1.0 gives this spec full weight in aggregation
      :value 1.0))))


;;; Utility function also used elsewhere

(defun org-window-habit-parse-only-days (str)
  "Parse ONLY_DAYS property string STR into a list of day symbols.
STR should be a Lisp list like (:monday :wednesday :friday)."
  (when str
    (car (read-from-string str))))

(provide 'org-window-habit-instance)
;;; org-window-habit-instance.el ends here
