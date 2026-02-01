;;; org-window-habit-core.el --- Core classes and basic methods -*- lexical-binding: t; -*-

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

;; Core EIEIO class definitions and basic methods for org-window-habit.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'org-window-habit-time)
(require 'org-window-habit-config)

;; Forward declarations
(declare-function org-window-habit-property "org-window-habit")


;;; Utility functions

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


;;; EIEIO Classes

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
   (reschedule-days
    :initarg :reschedule-days :initform nil
    :documentation "List of day symbols restricting when the habit can be rescheduled.
Example: (:monday :tuesday :wednesday :thursday :friday) for weekdays only.
Unlike only-days, this does NOT affect which completions count - only when
reminders appear. If both only-days and reschedule-days are set, the effective
reschedule days are the intersection (reschedule-days must be subset of only-days).
Nil means use only-days for rescheduling (backward compatible).")
   (start-time
    :initarg :start-time :initform nil
    :documentation "Anchor time for assessment interval boundaries.
Computed from earliest completion or reset-time if not specified.")
   (configs
    :initarg :configs :initform nil
    :documentation "List of parsed versioned config plists with resolved dates.
Each config plist contains :window-specs, :from, :until, and other habit parameters.
Used for habits with evolving requirements over time."))
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
   (conforming-baseline
    :initarg :conforming-baseline :initform 1.0
    :documentation "Fraction of target that counts as fully conforming.
A value of 0.8 means 80% of target-repetitions gives a ratio of 1.0.
Default 1.0 means the full target is required for 100% conforming.")
   (max-conforming-ratio
    :initarg :max-conforming-ratio :initform 1.0
    :documentation "Maximum conforming ratio (allows extra credit above 1.0).
A value of 1.2 allows up to 20% extra credit when exceeding the baseline.
Default 1.0 caps at 100% conforming (no extra credit).")
   (habit
    :initarg :habit
    :documentation "Back-reference to the parent `org-window-habit' object.
Set automatically during habit initialization."))
  "Specification for a single evaluation window within a habit.")

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


;;; Habit initialization

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


;;; Assessment window methods

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


;;; Habit methods

(cl-defmethod org-window-habit-get-effective-start ((habit org-window-habit))
  "Get the effective start time for HABIT.
If HABIT has versioned configs, returns the :from of the oldest config.
Otherwise returns the reset-time slot value.
Returns nil if there's no effective start (unbounded past)."
  (if (oref habit configs)
      (org-window-habit-configs-get-effective-start (oref habit configs))
    (oref habit reset-time)))

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

(cl-defmethod org-window-habit-has-any-done-times ((habit org-window-habit))
  "Return non-nil if HABIT has any recorded completions."
  (> (length (oref habit done-times)) 0))


;;; Window spec methods

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

(cl-defmethod org-window-habit-get-assessment-window
  ((spec org-window-habit-window-spec) time)
  "Get the assessment window for SPEC at TIME."
  (funcall (or (oref spec find-window)
               'org-window-habit-get-window-where-time-in-last-assessment)
           spec time))

(provide 'org-window-habit-core)
;;; org-window-habit-core.el ends here
