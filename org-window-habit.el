;;; org-window-habit.el --- Time window based habits -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: calendar org-mode habit interval window
;; URL: https://github.com/colonelpanic8/org-window-habit
;; Version: 0.1.0
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

(require 'eieio)
(require 'calendar)
(require 'org)
(require 'org-habit)
(require 'org-agenda)
(require 'cl-lib)

(defgroup org-window-habit nil
  "Customization options for `org-window-habit'."
  :group 'org-habit)

(defcustom org-window-habit-property-prefix "OWH"
  "The prefix that will be used when finding org properties for `org-window-habit'."
  :group 'org-window-habit
  :type 'string)

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

(defcustom org-window-habit-completion-needed-today-glyph ?☐
  "Glyph character used to show intervals in which a completion is expected."
  :group 'org-window-habit
  :type 'character)

(defcustom org-window-habit-completed-glyph ?✓
  "Glyph character used for the current interval when the habit has been completed."
  :group 'org-window-habit
  :type 'character)

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

(defcustom org-window-habit-repeat-to-deadline t
  "Reassign the deadline of habits on repeat."
  :group 'org-window-habit
  :type 'boolean)

(defcustom org-window-habit-repeat-to-scheduled nil
  "Reassign the scheduled field of habits on repeat."
  :group 'org-window-habit
  :type 'boolean)


;; Utility functions

(defun org-window-habit-property (name)
  "Return the full property name for NAME with the configured prefix.
For example, if prefix is \"OWH\" and NAME is \"WINDOW_DURATION\",
returns \"OWH_WINDOW_DURATION\"."
  (if org-window-habit-property-prefix
      (format "%s_%s" org-window-habit-property-prefix name)
    name))

(defun org-window-habit-entry-p ()
  "Return non-nil if entry at point is an org-window-habit.
An entry is considered a window habit if it has either:
- The WINDOW_SPECS property (new format), or
- The WINDOW_DURATION property (simple format)
Property names respect `org-window-habit-property-prefix'."
  (and (org-entry-get nil "TODO")
       (or (org-entry-get nil (org-window-habit-property "WINDOW_SPECS") t)
           (org-entry-get nil (org-window-habit-property "WINDOW_DURATION") t))))

(defun org-window-habit-time-to-string (time)
  "Format TIME as \"YYYY-MM-DD HH:MM\" for display and debugging."
  (format-time-string
   "%F %R"
   time))

(defun org-window-habit-time-max (&rest args)
  "Return the maximum time value from ARGS."
  (let ((max-time (car args)))
    (dolist (time (cdr args))
      (when (time-less-p max-time time)
        (setq max-time time)))
    max-time))

(defun org-window-habit-negate-plist (plist)
  "Negate all numeric values in duration PLIST.
Used to create a decrement plist for iterating backwards through time."
  (org-window-habit-multiply-plist plist -1))

(defun org-window-habit-multiply-plist (plist factor)
  "Multiply all numeric values in duration PLIST by FACTOR.
For example, (:days 2 :hours 3) with FACTOR 2 becomes (:days 4 :hours 6)."
  (cl-loop for v in plist
           for index from 0
           collect (if (eq (mod index 2) 1)
                       (* factor v)
                     v)))

(defun org-window-habit-duration-proportion (start-time end-time between-time)
  "Calculate proportion of interval from START-TIME to END-TIME remaining.
Returns (END-TIME - BETWEEN-TIME) / (END-TIME - START-TIME), 0.0 to 1.0.
Used to scale targets when a habit hasn't been active for the full window."
  (let* ((full-interval (float-time (time-subtract end-time start-time)))
         (partial-interval (float-time (time-subtract end-time between-time))))
    (/ partial-interval full-interval)))

(cl-defun org-window-habit-keyed-duration-add
    (&key (base-time (current-time))
          (days 0) (weeks 0) (months 0) (years 0)
          (hours 0) (minutes 0) (seconds 0)
          start)  ; ignored here, only used for normalization
  "Add a duration to BASE-TIME and return the resulting time.
DAYS, WEEKS, MONTHS, YEARS, HOURS, MINUTES, and SECONDS specify the duration.
Supports calendar-aware arithmetic for months and years.
The START parameter is accepted but ignored (used only in normalization)."
  (ignore start)
  (let* ((decoded-base (decode-time base-time))
         (base-year (nth 5 decoded-base))
         (base-month (nth 4 decoded-base))
         (base-day (nth 3 decoded-base))
         (base-absolute (calendar-absolute-from-gregorian (list base-month base-day base-year)))
         (new-absolute (+ base-absolute days (* weeks 7)))
         (gregorian-result (calendar-gregorian-from-absolute new-absolute))
         (result-year (+ (caddr gregorian-result) years))
         (result-month (+ (car gregorian-result) months)))

    ;; Handle month overflows and underflows
    (while (> result-month 12)
      (setq result-month (- result-month 12)
            result-year (+ result-year 1)))

    (while (< result-month 1)
      (setq result-month (+ result-month 12)
            result-year (- result-year 1)))

    (encode-time (+ seconds (nth 0 decoded-base))
                 (+ minutes (nth 1 decoded-base))
                 (+ hours (nth 2 decoded-base))
                 (cadr gregorian-result)
                 result-month
                 result-year)))

(defun org-window-habit-keyed-duration-add-plist (base-time plist)
  "Add duration PLIST to BASE-TIME.
PLIST is a property list like (:days 7) or (:months 1 :days 15)."
  (apply #'org-window-habit-keyed-duration-add :base-time base-time plist))

(cl-defun org-window-habit-string-duration-to-plist
    (string-value &key (default nil))
  "Parse STRING-VALUE into a duration plist, or return DEFAULT if nil.
Supports formats like \"1w\", \"2d\", \"3h\", \"1m\", \"1y\" or plists."
  (if (null string-value)
      default
    (let ((read-value (read string-value)))
      (cond
       ((plistp read-value) read-value)
       ((string-match "\\([0-9]+\\)[Yy]" string-value)
        (list :years (string-to-number (match-string 1 string-value))))

       ;; Month pattern
       ((string-match "\\([0-9]+\\)[Mm]" string-value)
        (list :months (string-to-number (match-string 1 string-value))))

       ;; Week pattern
       ((string-match "\\([0-9]+\\)[Ww]" string-value)
        (list :days (* 7 (string-to-number (match-string 1 string-value)))))

       ;; Day pattern
       ((string-match "\\([0-9]+\\)[Dd]" string-value)
        (list :days (string-to-number (match-string 1 string-value))))

       ;; Hour pattern
       ((string-match "\\([0-9]+\\)[Hh]" string-value)
        (list :hours (string-to-number (match-string 1 string-value))))
       (t (list :days read-value))))))

(defun org-window-habit-day-of-week-number (day-symbol)
  "Convert DAY-SYMBOL to a number (0=Sunday, 1=Monday, ..., 6=Saturday)."
  (pcase day-symbol
    (:sunday 0)
    (:monday 1)
    (:tuesday 2)
    (:wednesday 3)
    (:thursday 4)
    (:friday 5)
    (:saturday 6)
    (_ (error "Invalid day symbol: %s" day-symbol))))

(defun org-window-habit-day-of-week-symbol (day-number)
  "Convert DAY-NUMBER (0=Sunday, ..., 6=Saturday) to a keyword symbol."
  (nth day-number '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)))

(defun org-window-habit-time-on-allowed-day-p (time only-days)
  "Return non-nil if TIME falls on one of ONLY-DAYS.
ONLY-DAYS is a list of day symbols like (:monday :wednesday :friday).
If ONLY-DAYS is nil, all days are allowed."
  (or (null only-days)
      (let ((day-of-week (org-window-habit-day-of-week-symbol
                          (nth 6 (decode-time time)))))
        (memq day-of-week only-days))))

(defun org-window-habit-next-allowed-day (time only-days)
  "Return TIME if it's on an allowed day, otherwise the next allowed day.
If ONLY-DAYS is nil, returns TIME unchanged."
  (if (or (null only-days)
          (org-window-habit-time-on-allowed-day-p time only-days))
      time
    ;; Find next allowed day by advancing one day at a time (max 7 iterations)
    (cl-loop for i from 1 to 7
             for next-time = (org-window-habit-keyed-duration-add-plist time (list :days i))
             when (org-window-habit-time-on-allowed-day-p next-time only-days)
             return next-time
             finally return time)))

(defun org-window-habit-normalize-time-to-duration
    (time-value duration-plist)
  "Normalize TIME-VALUE to the start of a period defined by DURATION-PLIST.
For :days, aligns to midnight. For :hours, aligns to the hour boundary.
For :weeks with :start, aligns to the specified day of week.
For :months, aligns to the 1st of the month."
  (let* ((alignment-decoded (decode-time time-value))
         (year (nth 5 alignment-decoded))
         (month (nth 4 alignment-decoded))
         (day (nth 3 alignment-decoded))
         (hour (nth 2 alignment-decoded))
         (minute (nth 1 alignment-decoded))
         (second (nth 0 alignment-decoded))
         (day-of-week (nth 6 alignment-decoded))
         ;; Check for :weeks with optional :start
         (weeks-value (plist-get duration-plist :weeks))
         (week-start-day (or (plist-get duration-plist :start) :monday))
         ;; For non-week durations, find smallest duration type
         (smallest-duration-type (if weeks-value :weeks (car (last duration-plist 2))))
         (smallest-duration-value (if weeks-value weeks-value (cadr (last duration-plist 2)))))

    ;; Align time based on the smallest duration type and its value
    (cond
     ((eq smallest-duration-type :seconds)
      (encode-time
       (* smallest-duration-value (floor second smallest-duration-value)) minute
       hour day month year))

     ((eq smallest-duration-type :minutes)
      (encode-time
       0 (* smallest-duration-value
            (floor minute smallest-duration-value)) hour day month year))

     ((eq smallest-duration-type :hours)
      (encode-time
       0 0 (* smallest-duration-value (floor hour smallest-duration-value))
       day month year))

     ((eq smallest-duration-type :days)
      (let* ((aligned-day (- day (1- smallest-duration-value))))
        (encode-time 0 0 0 aligned-day month year)))

     ((eq smallest-duration-type :weeks)
      (let* ((target-dow (org-window-habit-day-of-week-number week-start-day))
             ;; Calculate days to subtract to reach the target day-of-week
             (days-since-target (mod (- day-of-week target-dow) 7))
             (aligned-day (- day days-since-target)))
        (encode-time 0 0 0 aligned-day month year)))

     ((eq smallest-duration-type :months)
      (encode-time 0 0 0 1
                   (* smallest-duration-value (floor month smallest-duration-value))
                   year))

     ((eq smallest-duration-type :years)
      (let* ((aligned-year (- year (1- smallest-duration-value))))
        (encode-time 0 0 0 1 1 aligned-year)))

     (t time-value))))

(defun org-window-habit-find-aligned-bounding-time (time-value duration-plist aligned-time)
  "Find the bounding ALIGNED-TIME for TIME-VALUE using DURATION-PLIST.
Iterates backwards from ALIGNED-TIME until it no longer exceeds TIME-VALUE."
  (let (new-time)
    (while (time-less-p time-value aligned-time)
      (setq new-time
            (org-window-habit-keyed-duration-add-plist aligned-time duration-plist))
      (when (not (time-less-p new-time aligned-time))
        (error "Time did not decrease in alignment attempt"))
      (setq aligned-time new-time)))
  aligned-time)

(defun org-window-habit-logbook-drawer-bounds ()
  "Return the bounds (start end) of the LOGBOOK drawer at point, or nil."
  (when (re-search-forward org-logbook-drawer-re nil t)
    (list (match-beginning 0) (match-end 0))))

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

(defun org-window-habit-time-less-or-equal-p (time1 time2)
  "Return non-nil if TIME1 is less than or equal to TIME2."
  (or (time-less-p time1 time2)
      (time-equal-p time1 time2)))

(defun org-window-habit-time-greater-p (time1 time2)
  "Return non-nil if TIME1 is strictly greater than TIME2."
  (time-less-p time2 time1))

(defun org-window-habit-time-greater-or-equal-p (time1 time2)
  "Return non-nil if TIME1 is greater than or equal to TIME2."
  (org-window-habit-time-less-or-equal-p time2 time1))

(defun org-window-habit-maybe-make-list-of-lists (value)
  "Ensure VALUE is a list of lists.
If VALUE is already a list of lists, return it unchanged.
Otherwise, wrap it in another list.  Used for graph building."
  (if (and (listp value) (listp (car value)))
      value
    (list value)))

(defun org-window-habit-default-aggregation-fn (collection)
  "Return the minimum conforming ratio from COLLECTION.
COLLECTION is a list of (ratio value window) tuples.
Used when multiple window specs must all be satisfied."
  (cl-loop for el in collection minimize (car el)))


;; Data types

(defclass org-window-habit ()
  ((window-specs
    :initarg :window-specs :initform nil
    :documentation "List of `org-window-habit-window-spec' objects defining evaluation windows.")
   (assessment-interval
    :initarg :assessment-interval :initform '(:days 1)
    :documentation "Duration plist for how often to re-evaluate conformity (step size for rolling window).")
   (reschedule-interval
    :initarg :reschedule-interval :initform '(:days 1)
    :documentation "Minimum time after completion before the habit can be rescheduled.")
   (reschedule-threshold
    :initarg :reschedule-threshold :initform 1.0
    :documentation "Conforming ratio threshold for rescheduling.
When the ratio drops below this value, the habit needs completion.
Default 1.0 means reschedule as soon as not fully conforming.")
   (done-times
    :initarg :done-times :initform nil
    :documentation "Vector of completion timestamps in descending order (most recent first).")
   (assessment-decrement-plist
    :initarg :assessment-decrement-plist :initform nil
    :documentation "Negated assessment-interval, used for iterating backwards through time.")
   (max-repetitions-per-interval
    :initarg :max-repetitions-per-interval :initform 1
    :documentation "Maximum completions counted per assessment interval.
Default 1 prevents multiple same-day completions from counting extra.")
   (aggregation-fn
    :initarg :aggregation-fn :initform 'org-window-habit-default-aggregation-fn
    :documentation "Function to aggregate conforming values across multiple window specs.
Takes a list of (ratio value window) and returns the aggregate ratio.")
   (graph-assessment-fn
    :initarg :graph-assessment-fn :initform nil
    :documentation "Function to determine graph character and color for each interval.
If nil, uses `org-window-habit-graph-assessment-fn'.")
   (reset-time
    :initarg :reset-time :initform nil
    :documentation "Timestamp before which completions are ignored (for fresh starts).")
   (only-days
    :initarg :only-days :initform nil
    :documentation "List of day symbols restricting when the habit applies.
Example: (:monday :wednesday :friday). Nil means all days allowed.")
   (start-time
    :initarg :start-time :initform nil
    :documentation "Anchor time for assessment interval boundaries.
Computed from earliest completion or reset-time if not specified."))
  "A window-based habit with configurable evaluation windows and conformity tracking.")

(defclass org-window-habit-window-spec ()
  ((duration-plist
    :initarg :duration :initform '(:days 1)
    :documentation "Duration plist defining how far back to look for completions.")
   (target-repetitions
    :initarg :repetitions :initform 1
    :documentation "Number of completions required within the window duration.")
   (conforming-value
    :initarg :value :initform nil
    :documentation "Weight of this spec when aggregating multiple windows.
If nil, uses duration-plist for ordering.")
   (find-window
    :initarg :find-window :initform nil
    :documentation "Custom function to compute assessment window for a given time.
If nil, uses `org-window-habit-get-window-where-time-in-last-assessment'.")
   (habit
    :initarg :habit
    :documentation "Back-reference to the parent `org-window-habit' object.
Set automatically during habit initialization."))
  "Specification for a single evaluation window within a habit.")

(cl-defmethod initialize-instance :after ((habit org-window-habit) &rest _args)
  "Initialize HABIT after slot values are set.
Validates required properties and sets up defaults."
  (when (null (oref habit assessment-interval))
    (error "Habits must have the %s property set when org-window-habit is enabled"
           (org-window-habit-property "ASSESSMENT_INTERVAL")))
  (when (null (oref habit window-specs))
    (error "Habits must define at least one window when org-window-habit is enabled"))
  (when (null (oref habit reschedule-interval))
    (oset habit reschedule-interval (oref habit assessment-interval)))
  (when (null (oref habit assessment-decrement-plist))
    (oset habit assessment-decrement-plist
          (org-window-habit-negate-plist (oref habit assessment-interval))))
  (when (null (oref habit start-time))
    (oset habit start-time
          (org-window-habit-normalize-time-to-duration
           (or (oref habit reset-time)
               (org-window-habit-earliest-completion-after-reset habit)
               (current-time))
           (oref habit assessment-interval))))
  (cl-loop for window-spec in (oref habit window-specs)
           do (oset window-spec habit habit)))

(defclass org-window-habit-assessment-window ()
  ((assessment-start-time
    :initarg :assessment-start-time
    :documentation "Start of the current assessment interval (when we re-evaluate).")
   (assessment-end-time
    :initarg :assessment-end-time
    :documentation "End of the current assessment interval.")
   (start-time
    :initarg :start-time
    :documentation "Start of the data window (how far back we look for completions).")
   (end-time
    :initarg :end-time
    :documentation "End of the data window (same as assessment-end-time)."))
  "Represents a time window for evaluating habit conformity.
The assessment interval [assessment-start-time, assessment-end-time) is when
we evaluate, while [start-time, end-time) is the range we look for completions.")

(cl-defmethod org-window-habit-time-falls-in-assessment-interval
    ((window org-window-habit-assessment-window) time)
  "Return non-nil if TIME falls within WINDOW's assessment interval."
  (and
   (time-less-p
    time
    (oref window assessment-end-time))
   (org-window-habit-time-less-or-equal-p
    (oref window assessment-start-time) time)))

(cl-defmethod org-window-habit-assesment-window-string
  ((window org-window-habit-assessment-window))
  "Return a string representation of WINDOW for debugging."
  (with-slots (assessment-start-time assessment-end-time start-time end-time) window
    (format
     "%s %s %s %s"
     (format "assessment-start-time: %s" (org-window-habit-time-to-string assessment-start-time))
     (format "assessment-end-time: %s" (org-window-habit-time-to-string assessment-end-time))
     (format "start-time: %s" (org-window-habit-time-to-string start-time))
     (format "end-time: %s" (org-window-habit-time-to-string end-time)))))

(defun org-window-habit-parse-only-days (str)
  "Parse ONLY_DAYS property string STR into a list of day symbols.
STR should be a Lisp list like (:monday :wednesday :friday)."
  (when str
    (car (read-from-string str))))

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an `org-window-habit' instance from the current org entry."
  (save-excursion
    (let* ((done-times
            (cl-loop for state-change-info in (org-window-habit-parse-logbook)
                     if (member (nth 0 state-change-info) org-done-keywords)
                     collect (nth 2 state-change-info)))
           (done-times-vector (vconcat done-times))
           (assessment-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get
              nil (org-window-habit-property "ASSESSMENT_INTERVAL")) :default '(:days 1)))
           (reschedule-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil (org-window-habit-property "RESCHEDULE_INTERVAL"))))
           (max-repetitions-per-interval
            (string-to-number
             (or (org-entry-get
                  nil (org-window-habit-property "MAX_REPETITIONS_PER_INTERVAL") t) "1")))
           (reset-time-str
            (org-entry-get nil (org-window-habit-property "RESET_TIME")))
           (reset-time
            (when reset-time-str
              (org-time-string-to-time reset-time-str)))
           (only-days
            (org-window-habit-parse-only-days
             (org-entry-get nil (org-window-habit-property "ONLY_DAYS")))))
      (make-instance 'org-window-habit
                     :start-time nil
                     :reset-time reset-time
                     :only-days only-days
                     :window-specs (or
                                    (org-window-habit-create-specs)
                                    (org-window-habit-create-specs-from-perfect-okay))
                     :assessment-interval assessment-interval
                     :reschedule-interval reschedule-interval
                     :done-times done-times-vector
                     :max-repetitions-per-interval max-repetitions-per-interval))))

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


;; Iterator

(defclass org-window-habit-iterator ()
  ((window-spec
    :initarg :window-spec
    :documentation "The `org-window-habit-window-spec' this iterator is evaluating.")
   (window
    :initarg :window
    :documentation "Current `org-window-habit-assessment-window' being evaluated.")
   (start-index
    :initarg :start-index
    :documentation "Index into done-times vector for window start (inclusive).")
   (end-index
    :initarg :end-index
    :documentation "Index into done-times vector for window end (exclusive)."))
  "Iterator for stepping through assessment windows of a habit.
Maintains position in the done-times array for efficient traversal.")

(cl-defun org-window-habit-iterator-from-time (window-spec &optional time)
  "Create an iterator for WINDOW-SPEC positioned at TIME.
TIME defaults to current time.  Returns an iterator ready for evaluation."
  (setq time (or time (current-time)))
  (let* ((iterator
          (make-instance 'org-window-habit-iterator
                         :window-spec window-spec
                         :window (org-window-habit-get-assessment-window window-spec time)
                         :start-index 0
                         :end-index 0)))
    (org-window-habit-adjust-iterator-indices iterator)
    iterator))

(cl-defmethod org-window-habit-advance
  ((iterator org-window-habit-iterator) &key (amount nil))
  "Move ITERATOR forward (or backward) by AMOUNT.
AMOUNT defaults to the habit's assessment-interval.
Updates the window and adjusts indices into the done-times array."
  (with-slots (window window-spec) iterator
    (unless amount
      (setq amount (oref (oref window-spec habit) assessment-interval)))
    (let*
        ((new-start-time (org-window-habit-keyed-duration-add-plist
                          (oref window assessment-start-time)
                          amount))
         (window-moved-backward
          (time-less-p new-start-time (oref window assessment-start-time)))
         (new-window (org-window-habit-get-assessment-window window-spec new-start-time)))
      (oset iterator window new-window)
      (org-window-habit-adjust-iterator-indices
       iterator (not window-moved-backward)))))

(cl-defmethod org-window-habit-adjust-iterator-indices
  ((iterator org-window-habit-iterator)
   &optional window-moved-forward)
  "Update ITERATOR's indices to match its current window.
WINDOW-MOVED-FORWARD indicates search direction for efficiency.
Indices point into the habit's done-times vector."
  (with-slots (window start-index end-index window-spec) iterator
      (cl-destructuring-bind (new-start-index new-end-index)
          (org-window-habit-get-completion-window-indices
           (oref window-spec habit)
           (oref window start-time) (oref window end-time)
           :start-index start-index
           :end-index end-index
           :reverse window-moved-forward)
        (oset iterator start-index new-start-index)
        (oset iterator end-index new-end-index))))

(cl-defmethod org-window-habit-conforming-ratio
  ((iterator org-window-habit-iterator) &rest args)
  "Calculate the conforming ratio for ITERATOR's current window.
ARGS are passed to the completion counting function.
Returns completions / (scale * target), clamped to [0.0, 1.0].
A ratio of 1.0 means fully conforming; lower values indicate falling behind."
  (with-slots (window-spec window start-index) iterator
    (min
     1.0
     (/
      (apply #'org-window-habit-get-completion-count
             (oref window-spec habit)
             (oref window start-time)
             (oref window end-time)
             :start-index start-index
             args)
      (* (org-window-habit-actual-window-scale iterator)
         (oref window-spec target-repetitions))))))

(cl-defmethod org-window-habit-actual-window-scale
  ((iterator org-window-habit-iterator))
  "Calculate the effective window scale for ITERATOR.
Returns a value between 0.0 and 1.0 representing what fraction of
the window is actually active (accounts for habit start time)."
  (with-slots (window) iterator
    (org-window-habit-duration-proportion
     (oref window start-time) (oref window end-time)
     (org-window-habit-effective-start iterator))))

(cl-defmethod org-window-habit-get-conforming-value
  ((iterator org-window-habit-iterator) &rest args)
  "Return (ratio duration-plist window) for ITERATOR.
ARGS are passed to the conforming ratio calculation."
  (with-slots (window-spec window) iterator
    (list (apply #'org-window-habit-conforming-ratio iterator args)
          (or (oref window-spec conforming-value)
              (oref window-spec duration-plist))
          window)))


;; Default versions of customizable functions

(defun org-window-habit-duration-plist-to-seconds (plist)
  "Convert a duration PLIST to approximate seconds.
Only works for fixed-length durations (:days, :hours, :minutes, :seconds).
Returns nil for variable-length durations (:months, :years, :weeks with :start)."
  (let ((days (or (plist-get plist :days) 0))
        (hours (or (plist-get plist :hours) 0))
        (minutes (or (plist-get plist :minutes) 0))
        (seconds (or (plist-get plist :seconds) 0))
        (weeks (plist-get plist :weeks))
        (months (plist-get plist :months))
        (years (plist-get plist :years)))
    ;; Return nil for variable-length or week-aligned durations
    (if (or months years weeks)
        nil
      ;; Convert to seconds: 86400 = seconds/day, 3600 = seconds/hour, 60 = seconds/minute
      (+ (* days 86400)
         (* hours 3600)
         (* minutes 60)
         seconds))))

(defun org-window-habit-get-anchored-assessment-start (anchor-time current-time assessment-plist)
  "Get assessment start time anchored to ANCHOR-TIME containing CURRENT-TIME.
ASSESSMENT-PLIST defines the interval length.
For fixed-length intervals (days, hours), calculates which assessment period
CURRENT-TIME falls into relative to ANCHOR-TIME.
For variable-length intervals (months, weeks), falls back to calendar alignment."
  (let ((interval-seconds (org-window-habit-duration-plist-to-seconds assessment-plist)))
    (if interval-seconds
        ;; Fixed-length interval: calculate based on seconds since anchor
        (let* ((anchor-seconds (float-time anchor-time))
               (current-seconds (float-time current-time))
               (elapsed (- current-seconds anchor-seconds))
               (periods (floor (/ elapsed interval-seconds)))
               (assessment-start-seconds (+ anchor-seconds (* periods interval-seconds))))
          (seconds-to-time assessment-start-seconds))
      ;; Variable-length interval: use calendar-based normalization
      (org-window-habit-normalize-time-to-duration current-time assessment-plist))))

(defun org-window-habit-get-window-where-time-in-last-assessment (spec time)
  "Return assessment window for SPEC containing TIME."
  (let* ((habit (oref spec habit))
         (assessment-plist (oref habit assessment-interval))
         (habit-start (oref habit start-time))
         (assessment-start
          (org-window-habit-get-anchored-assessment-start
           habit-start time assessment-plist))
         (assessment-end
          (org-window-habit-keyed-duration-add-plist
           assessment-start
           assessment-plist))
         (window-start
          (org-window-habit-keyed-duration-add-plist
           assessment-end
           (org-window-habit-negate-plist (oref spec duration-plist)))))
    (make-instance
     'org-window-habit-assessment-window
     :assessment-start-time assessment-start
     :assessment-end-time assessment-end
     :start-time window-start
     :end-time assessment-end)))


;; Datatype utility

(cl-defmethod org-window-habit-get-assessment-window
  ((spec org-window-habit-window-spec) time)
  "Get the assessment window for SPEC at TIME."
  (funcall (or (oref spec find-window)
               'org-window-habit-get-window-where-time-in-last-assessment)
           spec time))

(cl-defmethod org-window-habit-earliest-completion ((habit org-window-habit))
  "Return the earliest completion time for HABIT, or nil if none."
  (with-slots (done-times) habit
    (let ((done-times-count (length done-times)))
      (when (> done-times-count 0)
        (aref done-times (- done-times-count 1))))))

(cl-defmethod org-window-habit-earliest-completion-after-reset ((habit org-window-habit))
  "Return the earliest completion time for HABIT that is after the reset time.
If there is no reset time, return the earliest completion.
If there are no completions after reset, return nil."
  (with-slots (done-times reset-time) habit
    (if (null reset-time)
        (org-window-habit-earliest-completion habit)
      ;; done-times is in descending order (most recent first)
      ;; Iterate from oldest to newest, return first element >= reset-time
      (cl-loop for i from (1- (length done-times)) downto 0
               for time = (aref done-times i)
               when (org-window-habit-time-less-or-equal-p reset-time time)
               return time))))

(cl-defmethod org-window-habit-effective-start ((iterator org-window-habit-iterator))
  "Return the effective start time for ITERATOR's window."
  (org-window-habit-time-max (oref (oref iterator window) start-time)
                             (oref (oref (oref iterator window-spec) habit) start-time)))


;; Scanning done times

(cl-defun org-window-habit-find-array-forward
    (array time &key (start-index nil) (comparison '<))
  "Find index in ARRAY where TIME fits according to COMPARISON.
ARRAY is searched forward from START-INDEX.  Returns the first index
where COMPARISON returns nil."
  (setq start-index (or start-index 0))
  (cl-loop for index from start-index to (length array)
           while (and (< index (length array))
                      (funcall comparison time (aref array index)))
           finally return index))

(cl-defun org-window-habit-find-array-backward
    (array time &key (start-index nil) (comparison '<))
  "Find index in ARRAY where TIME fits according to COMPARISON.
ARRAY is searched backward from START-INDEX.  Returns the first index
where COMPARISON returns nil."
  (setq start-index (or start-index (length array)))
  (cl-loop for index downfrom start-index to 1
           for testing-value = (aref array (- index 1))
           while (funcall comparison time testing-value)
           finally return index))

(cl-defmethod org-window-habit-get-completion-window-indices
  ((habit org-window-habit) start-time end-time
   &key (start-index nil) (end-index nil) (reverse nil))
  "Get indices into HABIT's done-times for completions in time range.
Returns (start-index end-index) for completions between START-TIME
and END-TIME.  START-INDEX and END-INDEX are hints for optimization.
REVERSE indicates search direction."
  (with-slots (done-times) habit
    (if (not reverse)
        (list
         ;; We use end-time and not start time because the array is in descending
         ;; order
         (org-window-habit-find-array-forward
          done-times end-time
          :comparison 'org-window-habit-time-less-or-equal-p
          :start-index start-index)
         ;; We use start-time to compute the end index because the list is in
         ;; descending order
         (org-window-habit-find-array-forward
          done-times start-time
          :comparison 'org-window-habit-time-less-or-equal-p
          :start-index end-index))
      (list
       ;; We use end-time and not start time because the array is in descending
       ;; order
       (org-window-habit-find-array-backward
        done-times end-time
        :comparison 'org-window-habit-time-greater-p
        :start-index start-index)
       ;; We use start-time to compute the end index because the list is in
       ;; descending order
       (org-window-habit-find-array-backward
        done-times start-time
        :comparison 'org-window-habit-time-greater-p
        :start-index end-index)))))


;; Compute completions and required

(cl-defmethod org-window-habit-count-completions-in-index-range
  ((habit org-window-habit) start-idx end-idx)
  "Count completions in HABIT between START-IDX and END-IDX.
Filters by HABIT's only-days if set.  If only-days is nil, returns
the simple count (end-idx - start-idx)."
  (with-slots (done-times only-days) habit
    (if (null only-days)
        (- end-idx start-idx)
      (cl-loop for i from start-idx below end-idx
               for completion-time = (aref done-times i)
               count (org-window-habit-time-on-allowed-day-p completion-time only-days)))))

(cl-defmethod org-window-habit-get-completion-count
  ((habit org-window-habit) start-time end-time &key (start-index 0)
   (fill-completions-fn (lambda (_time actual-completions) actual-completions)))
  "Count completions for HABIT between START-TIME and END-TIME.
Respects max-repetitions-per-interval, reset-time, and only-days filters.
START-INDEX optimizes lookup in the done-times vector.
FILL-COMPLETIONS-FN allows modifying counts per interval (used for projections)."
  ;; Clamp start-time to not be before reset-time (if set)
  ;; This ensures completions before reset are not counted
  (let ((effective-start-time
         (if (oref habit reset-time)
             (org-window-habit-time-max start-time (oref habit reset-time))
           start-time)))
    (cl-loop
     with next-start-index = start-index
     with interval-end-time = end-time
     for interval-start-time =
     ;; This is just a sanity check for the case where the interval does not
     ;; evenly divide the window. But you shouldn't do that anyway.
     (org-window-habit-time-max
      effective-start-time
      (org-window-habit-keyed-duration-add-plist
       interval-end-time (oref habit assessment-decrement-plist)))
     for (start-index end-index) =
     (org-window-habit-get-completion-window-indices
      habit interval-start-time interval-end-time
      :start-index next-start-index
      :end-index next-start-index)
     for raw-completions = (org-window-habit-count-completions-in-index-range
                            habit start-index end-index)
     for completions-within-interval =
     (min (oref habit max-repetitions-per-interval)
          (funcall
           fill-completions-fn
           interval-start-time
           raw-completions))
     sum completions-within-interval
     do (setq next-start-index end-index
              interval-end-time interval-start-time)
     while (time-less-p effective-start-time interval-start-time))))

(cl-defmethod org-window-habit-get-next-required-interval
  ((habit org-window-habit) &optional now)
  "Find the next time when HABIT will need a completion.
Searches forward from NOW until the conforming ratio drops below
reschedule-threshold.  Respects only-days restrictions."
  (setq now (or now (current-time)))
  (with-slots
      (window-specs reschedule-interval reschedule-threshold assessment-interval
                    aggregation-fn done-times only-days)
      habit
    (let ((raw-result
           (if (org-window-habit-has-any-done-times habit)
               (cl-loop
                with start-time =
                (org-window-habit-normalize-time-to-duration
                 (org-window-habit-time-max
                  now
                  (org-window-habit-keyed-duration-add-plist (aref done-times 0)
                                                             reschedule-interval))
                 assessment-interval)
                with iterators =
                (cl-loop for window-spec in window-specs
                         collect
                         (org-window-habit-iterator-from-time window-spec start-time))
                for current-assessment-start = (oref (oref (car iterators) window) assessment-start-time)
                for conforming-values =
                (cl-loop for iterator in iterators
                         collect (org-window-habit-get-conforming-value iterator))
                for assessment-value = (funcall aggregation-fn conforming-values)
                until (< assessment-value reschedule-threshold)
                do
                (cl-loop for iterator in iterators
                         do (org-window-habit-advance iterator))
                finally return current-assessment-start)
             (org-window-habit-normalize-time-to-duration
              now assessment-interval))))
      ;; Snap to next allowed day if only-days is set
      (org-window-habit-next-allowed-day raw-result only-days))))

(cl-defmethod org-window-habit-assess-interval
  ((habit org-window-habit) iterators &rest args)
  "Compute aggregate conforming value for HABIT using ITERATORS.
ARGS are passed to the conforming value computation."
  (let* ((conforming-values
          (cl-loop for iterator in iterators
                   collect (apply #'org-window-habit-get-conforming-value iterator args))))
    (or (funcall (oref habit aggregation-fn) conforming-values) 0.0)))

(cl-defmethod org-window-habit-assess-interval-with-and-without-completions
  ((habit org-window-habit) iterators modify-completions-fn)
  "Assess HABIT's current interval with and without completions.
ITERATORS provide the window positions.  MODIFY-COMPLETIONS-FN adjusts
the completion count.
Returns (start end no-completions with-completions count)."
  (let* ((current-assessment-start
          (oref (oref (car iterators) window) assessment-start-time))
         (current-assessment-end
          (oref (oref (car iterators) window) assessment-end-time))
         (no-completions
          (org-window-habit-assess-interval
           habit iterators
           :fill-completions-fn
           (lambda (time actual-completions)
             (if (time-equal-p current-assessment-start time)
                 0
               actual-completions))))
         (with-completions
          (org-window-habit-assess-interval
           habit iterators
           :fill-completions-fn
           (lambda (time actual-completions)
             (if (time-equal-p current-assessment-start time)
                 (funcall modify-completions-fn actual-completions)
               actual-completions))))
         (count (org-window-habit-get-completion-count
                 habit current-assessment-start current-assessment-end)))
    (list current-assessment-start
          current-assessment-end
          no-completions
          with-completions
          count)))


;; Graph functions

(cl-defmethod org-window-habit-has-any-done-times ((habit org-window-habit))
  "Return non-nil if HABIT has any recorded completions."
  (> (length (oref habit done-times)) 0))

(cl-defmethod org-window-habit-build-graph ((habit org-window-habit) &optional now)
  "Build the consistency graph data for HABIT as of NOW.
Returns a list of (character face) pairs for each interval:
- Past intervals showing historical conformity
- Present interval showing current status
- Future intervals projecting expected conformity"
  (setq now (or now (current-time)))
  (with-slots
      (assessment-decrement-plist window-specs reschedule-interval
                                  max-repetitions-per-interval start-time aggregation-fn
                                  assessment-interval graph-assessment-fn)
      habit
    (unless graph-assessment-fn
      (setq graph-assessment-fn
            org-window-habit-graph-assessment-fn))
    (cl-destructuring-bind (actual-intervals actual-start-time)
        ;; Find the start time by going back by the assessment interval
        ;; `org-window-habit-preceding-intervals' times, or hitting the habits
        ;; absolute start.
        (cl-loop
         with target-start-time = (org-window-habit-normalize-time-to-duration
                                   now assessment-interval)
         for i from 0 to org-window-habit-preceding-intervals
         while (time-less-p start-time target-start-time)
         do
         (setq target-start-time
               (org-window-habit-keyed-duration-add-plist
                target-start-time
                assessment-decrement-plist))
         finally return (list i target-start-time))
      (nconc
       ;; Add filler if we don't have enough data to fill `org-window-habit-preceding-intervals'.
       (cl-loop for i from 0 to (- org-window-habit-preceding-intervals actual-intervals)
                collect (list ?\s 'default))
       (cl-loop
        with iterators =
        (cl-loop for window-spec in window-specs
                 collect
                 (org-window-habit-iterator-from-time
                  window-spec actual-start-time))
        while (time-less-p (oref (oref (car iterators) window) assessment-end-time) now)
        for
        (_current-assessment-start
         _current-assessment-end
         no-completions-assessment
         with-completions-assessment
         completion-in-interval-count) =
         (org-window-habit-assess-interval-with-and-without-completions
          habit iterators (lambda (x) x))
        collect
        (funcall
         graph-assessment-fn
         no-completions-assessment
         with-completions-assessment
         completion-in-interval-count
         'past
         habit
         (oref (car iterators) window))
        into past-assessments
        do
        (cl-loop for iterator in iterators
                 do (org-window-habit-advance iterator))
        finally
        return
        (cl-destructuring-bind
            (_current-assessment-start
             _current-assessment-end
             no-completions-assessment
             with-completions-assessment
             completion-in-interval-count)
            (org-window-habit-assess-interval-with-and-without-completions
             habit iterators (lambda (_x) max-repetitions-per-interval))
          (nconc past-assessments
                 ;; This is a hack to allow multi character stuff for the current day
                 (org-window-habit-maybe-make-list-of-lists
                  (funcall graph-assessment-fn no-completions-assessment
                           with-completions-assessment
                           completion-in-interval-count 'present habit
                           (oref (car iterators) window)))
                 (cl-loop
                  for i from 1 to org-window-habit-following-days
                  do
                  (cl-loop for iterator in iterators
                           do (org-window-habit-advance iterator))
                  for
                  (_current-assessment-start
                   _current-assessment-end
                   no-completions-assessment
                   with-completions-assessment
                   completion-in-interval-count) =
                   (org-window-habit-assess-interval-with-and-without-completions
                    habit iterators (lambda (_x) 0))
                   collect
                   (funcall
                    graph-assessment-fn
                    no-completions-assessment
                    with-completions-assessment
                    completion-in-interval-count
                    'future
                    habit
                    (oref (car iterators) window))))))))))

(defun org-window-habit-make-graph-string (graph-info)
  "Convert GRAPH-INFO into a propertized string for display.
GRAPH-INFO is a list of (character face) pairs."
  (let ((graph (make-string (length graph-info) ?\s)))
    (cl-loop for (character face) in graph-info
             for index from 0
             do
             (progn
               (aset graph index character)
               (put-text-property index (1+ index) 'face face graph)))
    graph))

(defun org-window-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks.
If LINE is provided, insert graphs at beggining of line"
  (let ((inhibit-read-only t)
	(buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p))
          (_invisible-prop (get-text-property (point) 'invisible)))
	  (when (and habit
                 (or (org-window-habit-has-any-done-times habit)
                     (progn (message "Skipping habit with no done times") nil)))
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			              (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-window-habit-make-graph-string
          (org-window-habit-build-graph habit))))
      ;; TODO: this should be reintroduced
      ;; Inherit invisible state of hidden entries.
      ;; (when invisible-prop
      ;;   (put-text-property
      ;;    (- (point) org-habit-graph-column) (point)
      ;;    'invisible invisible-prop))))
	  (forward-line))))))


;; Advice

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

(defun org-window-habit-auto-repeat (&rest _args)
  "Reassign the date of the habit to the next day at which it is required."
  (interactive)
  (let* ((required-interval-start
          (org-window-habit-get-next-required-interval
           (org-window-habit-create-instance-from-heading-at-point)))
         (repeat (org-get-repeat))
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
        (org-window-habit-add-repeater ".+1d")))))

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
  "Advice for habit auto-repeat.  Calls ORIG with ARGS, then triggers rescheduling."
  (let ((res (apply orig args)))
    (when (and org-window-habit-mode (org-is-habit-p))
      (apply #'org-window-habit-auto-repeat args))
    res))

(defun org-window-habit-add-log-note-advice (orig &rest args)
  "Advice for `org-add-log-note' that handles org-window-habit entries.
ORIG is the original function, ARGS are its arguments.
After the log note is added, this:
1. Fixes logbook order if entries are out of chronological order
2. Triggers the habit rescheduling logic"
  (let ((res (apply orig args)))
    (when org-window-habit-mode
      ;; Fix logbook order for org-window-habits
      (when (org-window-habit-entry-p)
        (org-window-habit-fix-logbook-order))
      ;; Trigger auto-repeat for all habits
      (when (org-is-habit-p)
        (apply #'org-window-habit-auto-repeat args)))
    res))


;; Default graph display functions

(defun org-window-habit-create-face (bg-color foreground-color)
  "Create or return a face with BG-COLOR background and FOREGROUND-COLOR.
Face names are cached based on color values to avoid recreating faces."
  (let* ((bg-name (replace-regexp-in-string "#" "" bg-color))
         (fg-name (replace-regexp-in-string "#" "" foreground-color))
         (face-name (intern (format "org-window-habit-face-bg-%s-fg-%s" bg-name fg-name))))
    (if (facep face-name)
        face-name
      (progn
        (make-face face-name)
        (set-face-attribute face-name nil :background bg-color :foreground foreground-color)
        face-name))))

(defun org-window-habit-rescale-assessment-value (value)
  "Apply non-conforming scale to VALUE if below 1.0.
Values at or above 1.0 (fully conforming) are returned unchanged."
  (if (>= value 1.0) value
    (*  org-window-habit-non-conforming-scale value)))

(defun org-window-habit-lerp-color (color1 color2 proportion)
  "Linearly interpolate between COLOR1 and COLOR2 by PROPORTION.
PROPORTION of 0.0 returns COLOR1, 1.0 returns COLOR2.
Colors should be hex strings like \"#RRGGBB\"."
  (let ((r1 (string-to-number (substring color1 1 3) 16))
        (g1 (string-to-number (substring color1 3 5) 16))
        (b1 (string-to-number (substring color1 5 7) 16))
        (r2 (string-to-number (substring color2 1 3) 16))
        (g2 (string-to-number (substring color2 3 5) 16))
        (b2 (string-to-number (substring color2 5 7) 16)))
    (format "#%02x%02x%02x"
            (round (+ (* (- r2 r1) proportion) r1))
            (round (+ (* (- g2 g1) proportion) g1))
            (round (+ (* (- b2 b1) proportion) b1)))))

(cl-defun org-window-habit-default-graph-assessment-fn
    (without-completion-assessment-value
     with-completion-assessment-value
     completions-in-interval
     current-interval-time-type
     habit
     window)
  "Determine graph character and face for an interval.
WITHOUT-COMPLETION-ASSESSMENT-VALUE is the ratio if no completion today.
WITH-COMPLETION-ASSESSMENT-VALUE is the ratio with today's completions.
COMPLETIONS-IN-INTERVAL is how many completions occurred in this interval.
CURRENT-INTERVAL-TIME-TYPE is \\='past, \\='present, or \\='future.
HABIT and WINDOW provide context for computing next-required-interval.
Returns (character face) or a list of such pairs for the present interval."
  (let* ((with-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            with-completion-assessment-value)))
         (without-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            without-completion-assessment-value)))
         (completion-today-matters
          (< without-completion-assessment-value with-completion-assessment-value))
         (next-required-interval (org-window-habit-get-next-required-interval habit))
         (completion-expected-today
          (org-window-habit-time-falls-in-assessment-interval
           window
           next-required-interval))
         (interval-has-completion (> completions-in-interval 0)))
    (pcase current-interval-time-type
      ('present
       (let* ((character
               (cond
                (interval-has-completion org-window-habit-completed-glyph)
                (completion-expected-today
                 org-window-habit-completion-needed-today-glyph)
                (t ?-)))
              (completion-face
               (org-window-habit-create-face with-completion-color "#000000"))
              (sample-face
               (if interval-has-completion
                   completion-face
                 (org-window-habit-create-face without-completion-color "#000000"))))
         (list
          (list ?| completion-face)
          (list character completion-face)
          (list ?\s sample-face)
          (list ?| completion-face))))
      ('past
       (let* ((bg-color
               (if completion-expected-today
                   without-completion-color
                 with-completion-color))
              (fg-color
               (cond
                (completion-expected-today
                 with-completion-color)
                (completion-today-matters
                 org-window-habit-required-completion-foreground-color)
                (t org-window-habit-non-required-completion-foreground-color)))
              (face (org-window-habit-create-face bg-color fg-color))
              (character
               (cond
                ((> completions-in-interval 0) org-habit-completed-glyph)
                (t ?\s))))
         (list character face)))
      ('future
       (let* ((bg-color
               without-completion-color)
              (fg-color
               "#000000")
              (face (org-window-habit-create-face bg-color fg-color))
              (character
               (cond
                (completion-expected-today
                 org-window-habit-completion-needed-today-glyph)
                (t ?\s))))
         (list character face))))))

(provide 'org-window-habit)
;;; org-window-habit.el ends here
