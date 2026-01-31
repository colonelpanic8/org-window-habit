;;; org-window-habit-config.el --- Configuration parsing and migration -*- lexical-binding: t; -*-

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

;; Configuration parsing for org-window-habit, including versioned config
;; support and migration utilities.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-window-habit-time)

;; Forward declaration for org-window-habit-property
(declare-function org-window-habit-property "org-window-habit")


;;; Date parsing

(defun org-window-habit-parse-config-date (date-value)
  "Parse DATE-VALUE into an Emacs time value.
DATE-VALUE can be:
- An org inactive timestamp: [2025-06-01] or [2025-06-01 Sun]
- An org active timestamp: <2025-06-01>
- A plain date string: \"2025-06-01\"
- nil (returns nil)
Returns a normalized Emacs time value at midnight."
  (when date-value
    (let* ((date-str
            (cond
             ;; Strip org timestamp brackets [...]
             ((string-match "^\\[\\([0-9]+-[0-9]+-[0-9]+\\)\\( [A-Za-z]+\\)?\\]$" date-value)
              (match-string 1 date-value))
             ;; Strip org active timestamp brackets <...>
             ((string-match "^<\\([0-9]+-[0-9]+-[0-9]+\\)\\( [A-Za-z]+\\)?>$" date-value)
              (match-string 1 date-value))
             ;; Plain date string
             ((string-match "^[0-9]+-[0-9]+-[0-9]+$" date-value)
              date-value)
             (t (error "Invalid date format: %s" date-value))))
           (parsed (parse-time-string date-str)))
      ;; parse-time-string returns (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
      ;; Build a time at midnight
      (encode-time 0 0 0
                   (nth 3 parsed)   ; day
                   (nth 4 parsed)   ; month
                   (nth 5 parsed))))) ; year


;;; Config structure detection

(defun org-window-habit-config-is-versioned-p (config-data)
  "Return non-nil if CONFIG-DATA is a versioned config (list of plists).
A single config is a bare plist starting with a keyword.
A versioned config is a list of plists (list starting with a list)."
  (and (listp config-data)
       (listp (car config-data))
       ;; First element is a list, not a keyword (which would indicate single plist)
       (not (keywordp (car config-data)))))


;;; Config parsing

(defun org-window-habit-parse-config (config-str)
  "Parse CONFIG-STR into a list of normalized config plists.
CONFIG-STR can be:
- A single config plist (bare plist)
- A versioned config (list of plists in reverse temporal order)

Returns a list of config plists with resolved :from and :until dates.
The first config in the list is the current (most recent) config.

Temporal chaining rules:
- Configs are in reverse temporal order (most recent first)
- First config: :until is implicitly nil (unbounded future)
- Each config's :from is implicitly the :until of the next config
- Explicit :from that differs from previous :until creates a gap
- Explicit :from on the last config acts like RESET_TIME"
  (let* ((config-data (car (read-from-string config-str)))
         (configs (if (org-window-habit-config-is-versioned-p config-data)
                      config-data
                    ;; Single config - wrap in list
                    (list config-data))))
    ;; Validate all configs have :window-specs
    (dolist (config configs)
      (unless (plist-get config :window-specs)
        (error "Config missing required :window-specs: %S" config)))
    ;; Process and chain configs
    (org-window-habit-chain-config-dates configs)))

(defun org-window-habit-chain-config-dates (configs)
  "Chain implicit :from/:until dates in CONFIGS.
CONFIGS is a list of config plists in reverse temporal order.
Returns a new list with resolved dates."
  (let ((result '()))
    ;; First pass: parse all dates and validate
    (setq result
          (cl-loop for config in configs
                   for explicit-from = (plist-get config :from)
                   for explicit-until = (plist-get config :until)
                   for parsed-from = (when explicit-from
                                       (if (stringp explicit-from)
                                           (org-window-habit-parse-config-date explicit-from)
                                         explicit-from))
                   for parsed-until = (when explicit-until
                                        (if (stringp explicit-until)
                                            (org-window-habit-parse-config-date explicit-until)
                                          explicit-until))
                   for new-config = (copy-sequence config)
                   do (setq new-config (plist-put new-config :from parsed-from))
                   do (setq new-config (plist-put new-config :until parsed-until))
                   collect new-config))
    ;; Validate temporal order: configs should be in reverse temporal order
    ;; (most recent first), so :until values should be decreasing
    (cl-loop for i from 0 below (1- (length result))
             for current = (nth i result)
             for next = (nth (1+ i) result)
             for current-until = (plist-get current :until)
             for next-until = (plist-get next :until)
             ;; If current config has no :until (unbounded future) but next has one,
             ;; that's correct order
             ;; If both have :until, current's should be >= next's (or equal)
             when (and current-until next-until
                       (time-less-p current-until next-until))
             do (error "Configs must be in reverse temporal order (most recent first)"))
    ;; Also validate: first config should NOT have :until (it's current/unbounded)
    ;; unless it's explicitly a bounded historical config
    (let ((first (car result)))
      (when (and (plist-get first :until)
                 (null (plist-get first :from))
                 (> (length result) 1))
        ;; First config has :until but no :from, and there are more configs
        ;; This means the "current" config has an end date, which is wrong order
        (error "Configs must be in reverse temporal order (most recent first)")))
    ;; Chain implicit :from values (each config's :from = next config's :until)
    ;; BUT only if the next config doesn't have an explicit :from that creates a gap
    (cl-loop for i from 0 below (length result)
             for config = (nth i result)
             for next-config = (when (< (1+ i) (length result))
                                 (nth (1+ i) result))
             ;; If this config doesn't have explicit :from, AND next config has :until,
             ;; AND next config does NOT have an explicit :from (which would indicate a gap)
             ;; then chain the :from
             when (and (null (plist-get config :from))
                       next-config
                       (plist-get next-config :until)
                       ;; Only chain if next config's range is unbounded at start
                       ;; (no explicit :from means continuous from before)
                       (null (plist-get next-config :from)))
             do (setf (nth i result)
                      (plist-put config :from (plist-get next-config :until))))
    result))


;;; Config access

(defun org-window-habit-get-config-for-time (configs time)
  "Return the active config from CONFIGS for the given TIME.
CONFIGS is a list of parsed config plists from
`org-window-habit-parse-config'.  Returns the matching config
plist, or nil if TIME falls in a gap or before the habit's start.

Boundary semantics: inclusive start, exclusive end.
A time exactly at a config's :from belongs to that config."
  (cl-loop for config in configs
           for config-from = (plist-get config :from)
           for config-until = (plist-get config :until)
           ;; Check if TIME is within this config's range
           ;; Range is [from, until) - inclusive start, exclusive end
           when (and
                 ;; TIME >= :from (or :from is nil meaning unbounded past)
                 (or (null config-from)
                     (org-window-habit-time-greater-or-equal-p time config-from))
                 ;; TIME < :until (or :until is nil meaning unbounded future)
                 (or (null config-until)
                     (time-less-p time config-until)))
           return config))

(defun org-window-habit-config-get-assessment-interval (config)
  "Get assessment-interval from CONFIG, defaulting to (:days 1) if not set."
  (or (plist-get config :assessment-interval)
      '(:days 1)))

(defun org-window-habit-config-get-max-reps-per-interval (config)
  "Get max-reps-per-interval from CONFIG, defaulting to 1 if not set."
  (or (plist-get config :max-reps-per-interval) 1))

(defun org-window-habit-config-get-reschedule-threshold (config)
  "Get reschedule-threshold from CONFIG, defaulting to 1.0 if not set."
  (or (plist-get config :reschedule-threshold) 1.0))

(defun org-window-habit-configs-get-effective-start (configs)
  "Get the effective start time from CONFIGS.
This is the :from of the oldest config (last in the list).
Returns nil if the oldest config has no :from (unbounded past)."
  (when configs
    (plist-get (car (last configs)) :from)))


;;; Migration utilities

(defun org-window-habit-migrate-to-config ()
  "Migrate current habit's scattered properties to unified CONFIG property.
Reads WINDOW_DURATION/REPETITIONS_REQUIRED or WINDOW_SPECS,
ASSESSMENT_INTERVAL, RESCHEDULE_INTERVAL, MAX_REPETITIONS_PER_INTERVAL,
ONLY_DAYS, and RESET_TIME properties, builds a unified config plist,
and writes it to the CONFIG property.

If RESET_TIME exists, it's converted to :from on the config."
  (interactive)
  (let* ((window-specs-str (org-entry-get nil (org-window-habit-property "WINDOW_SPECS") t))
         (window-duration (org-entry-get nil (org-window-habit-property "WINDOW_DURATION") t))
         (reps-required (org-entry-get nil (org-window-habit-property "REPETITIONS_REQUIRED") t))
         (assessment-str (org-entry-get nil (org-window-habit-property "ASSESSMENT_INTERVAL") t))
         (reschedule-str (org-entry-get nil (org-window-habit-property "RESCHEDULE_INTERVAL") t))
         (max-reps-str (org-entry-get nil (org-window-habit-property "MAX_REPETITIONS_PER_INTERVAL") t))
         (only-days-str (org-entry-get nil (org-window-habit-property "ONLY_DAYS") t))
         (reset-time-str (org-entry-get nil (org-window-habit-property "RESET_TIME")))
         ;; Build window-specs list
         (window-specs
          (if window-specs-str
              ;; Advanced format: use WINDOW_SPECS directly
              (car (read-from-string window-specs-str))
            ;; Simple format: build from WINDOW_DURATION + REPETITIONS_REQUIRED
            (let ((duration (org-window-habit-string-duration-to-plist
                             window-duration :default '(:days 1)))
                  (reps (string-to-number (or reps-required "1"))))
              (list (list :duration duration :repetitions reps)))))
         ;; Build config plist
         (config (list :window-specs window-specs)))
    ;; Add optional properties if present
    (when assessment-str
      (setq config (plist-put config :assessment-interval
                              (org-window-habit-string-duration-to-plist assessment-str))))
    (when reschedule-str
      (setq config (plist-put config :reschedule-interval
                              (org-window-habit-string-duration-to-plist reschedule-str))))
    (when max-reps-str
      (setq config (plist-put config :max-reps-per-interval
                              (string-to-number max-reps-str))))
    (when only-days-str
      (setq config (plist-put config :only-days
                              (car (read-from-string only-days-str)))))
    ;; Convert RESET_TIME to :from
    (when reset-time-str
      (setq config (plist-put config :from reset-time-str)))
    ;; Write CONFIG property
    (org-entry-put nil (org-window-habit-property "CONFIG")
                   (prin1-to-string config))
    (message "Migrated habit to CONFIG property")))

(defun org-window-habit-migrate-buffer ()
  "Migrate all habits in current buffer to CONFIG property format."
  (interactive)
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (when (and (org-window-habit-entry-p)
                  (not (org-entry-get nil (org-window-habit-property "CONFIG") t)))
         (org-window-habit-migrate-to-config)
         (cl-incf count))))
    (message "Migrated %d habits" count)))

(defun org-window-habit-insert-config-change (date)
  "Insert a new config version effective from DATE.
When called interactively, prompts for the date.
The current config becomes the new (future) config, and a copy is
created as the older config with :until set to DATE.

This converts a single config to versioned format if needed."
  (interactive
   (list (org-read-date nil t nil "Config change effective from: ")))
  (org-window-habit-insert-config-change-at-date date))

(defun org-window-habit-insert-config-change-at-date (date)
  "Insert a config change at DATE (a time value).
Creates a versioned config where the current config continues from DATE,
and the old config is preserved with :until DATE."
  (let* ((config-str (org-entry-get nil (org-window-habit-property "CONFIG") t))
         (date-str (format-time-string "%Y-%m-%d" date)))
    (if config-str
        ;; Have existing CONFIG property
        (let* ((config-data (car (read-from-string config-str)))
               (is-versioned (org-window-habit-config-is-versioned-p config-data))
               (configs (if is-versioned config-data (list config-data)))
               ;; Clone the current (first) config for the new entry
               (current-config (car configs))
               (new-current (copy-sequence current-config))
               (new-old (copy-sequence current-config)))
          ;; Set :from on new current config
          (setq new-current (plist-put new-current :from date-str))
          ;; Set :until on old config (which becomes the second entry)
          (setq new-old (plist-put new-old :until date-str))
          ;; Remove any :from from the old config (it should be unbounded or chained)
          (setq new-old (plist-put new-old :from nil))
          ;; Build new versioned config list
          (let ((new-configs (cons new-current (cons new-old (cdr configs)))))
            (org-entry-put nil (org-window-habit-property "CONFIG")
                           (prin1-to-string new-configs))))
      ;; No CONFIG - migrate first, then insert change
      (org-window-habit-migrate-to-config)
      (org-window-habit-insert-config-change-at-date date))
    (message "Inserted config change effective %s" date-str)))

;; Forward declaration for org-window-habit-entry-p
(declare-function org-window-habit-entry-p "org-window-habit")

(provide 'org-window-habit-config)
;;; org-window-habit-config.el ends here
