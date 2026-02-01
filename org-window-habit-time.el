;;; org-window-habit-time.el --- Time and duration utilities -*- lexical-binding: t; -*-

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

;; Time manipulation and duration parsing utilities for org-window-habit.

;;; Code:

(require 'calendar)
(require 'cl-lib)


;;; Time comparison functions

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


;;; Duration plist manipulation

(defun org-window-habit-negate-plist (plist)
  "Negate all numeric values in duration PLIST.
Used to create a decrement plist for iterating backwards through time."
  (org-window-habit-multiply-plist plist -1))

(defun org-window-habit-multiply-plist (plist factor)
  "Multiply all numeric values in duration PLIST by FACTOR.
For example, (:days 2 :hours 3) with FACTOR 2 becomes (:days 4 :hours 6).
Non-numeric values (like :monday in (:weeks 1 :start :monday)) are preserved."
  (cl-loop for v in plist
           for index from 0
           collect (if (and (eq (mod index 2) 1) (numberp v))
                       (* factor v)
                     v)))

(defun org-window-habit-duration-proportion (start-time end-time between-time)
  "Calculate proportion of interval from START-TIME to END-TIME remaining.
Returns (END-TIME - BETWEEN-TIME) / (END-TIME - START-TIME), 0.0 to 1.0.
Used to scale targets when a habit hasn't been active for the full window."
  (let* ((full-interval (float-time (time-subtract end-time start-time)))
         (partial-interval (float-time (time-subtract end-time between-time))))
    (/ partial-interval full-interval)))


;;; Duration arithmetic

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


;;; Duration parsing

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


;;; Day-of-week utilities

(defconst org-window-habit-weekdays
  '(:monday :tuesday :wednesday :thursday :friday)
  "List of weekday symbols for use with only-days and reschedule-days.")

(defconst org-window-habit-weekends
  '(:saturday :sunday)
  "List of weekend day symbols for use with only-days and reschedule-days.")

(defconst org-window-habit-all-days
  '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)
  "List of all day symbols.")

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

(defun org-window-habit-effective-reschedule-days (only-days reschedule-days)
  "Compute the effective days for rescheduling.
ONLY-DAYS and RESCHEDULE-DAYS are lists of allowed day numbers.
If RESCHEDULE-DAYS is nil, use ONLY-DAYS (backward compatible).
If ONLY-DAYS is nil, use RESCHEDULE-DAYS.
If both are set, return their intersection.
If both are nil, return nil (meaning all days allowed)."
  (cond
   ((and (null only-days) (null reschedule-days)) nil)
   ((null reschedule-days) only-days)
   ((null only-days) reschedule-days)
   ;; Both are set - return intersection
   (t (cl-intersection reschedule-days only-days))))


;;; Time normalization

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

(defun org-window-habit-get-anchored-assessment-start (anchor-time current-time assessment-plist)
  "Get assessment start time anchored to ANCHOR-TIME containing CURRENT-TIME.
ASSESSMENT-PLIST defines the interval length.

For day-based intervals, uses calendar arithmetic to correctly handle DST.
Adding N days means adding N calendar days, not N*86400 seconds, because
DST transition days are 23 or 25 hours.

For sub-day intervals (hours, minutes, seconds), uses seconds arithmetic
since these don't cross day boundaries where DST matters.

For variable-length intervals (months, weeks), falls back to calendar alignment."
  (let ((days (plist-get assessment-plist :days))
        (interval-seconds (org-window-habit-duration-plist-to-seconds assessment-plist)))
    (cond
     ;; Day-based intervals: use calendar arithmetic to avoid DST issues
     (days
      (let* ((anchor-decoded (decode-time anchor-time))
             (anchor-day (nth 3 anchor-decoded))
             (anchor-month (nth 4 anchor-decoded))
             (anchor-year (nth 5 anchor-decoded))
             (anchor-hour (nth 2 anchor-decoded))
             (anchor-minute (nth 1 anchor-decoded))
             (anchor-second (nth 0 anchor-decoded))
             ;; Convert both times to day numbers for integer arithmetic
             (anchor-day-number (time-to-days anchor-time))
             (current-day-number (time-to-days current-time))
             ;; Calculate how many complete intervals have passed
             (days-elapsed (- current-day-number anchor-day-number))
             (periods (floor (/ (float days-elapsed) days)))
             (total-days-to-add (* periods days))
             ;; Compute the target day by adding to anchor's calendar date
             (target-day (+ anchor-day total-days-to-add)))
        ;; encode-time handles day overflow (e.g., Jan 32 -> Feb 1)
        (encode-time anchor-second anchor-minute anchor-hour
                     target-day anchor-month anchor-year)))

     ;; Sub-day intervals: use seconds arithmetic (safe, no DST crossing)
     (interval-seconds
      (let* ((anchor-seconds (float-time anchor-time))
             (current-seconds (float-time current-time))
             (elapsed (- current-seconds anchor-seconds))
             (periods (floor (/ elapsed interval-seconds)))
             (assessment-start-seconds (+ anchor-seconds (* periods interval-seconds))))
        (seconds-to-time assessment-start-seconds)))

     ;; Variable-length intervals (months, weeks): use calendar normalization
     (t
      (org-window-habit-normalize-time-to-duration current-time assessment-plist)))))

(provide 'org-window-habit-time)
;;; org-window-habit-time.el ends here
