;;; org-window-habit-core.el --- Core classes and computation -*- lexical-binding: t; -*-

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

;; Core EIEIO classes, iterators, and completion counting for org-window-habit.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'org)
(require 'org-window-habit-time)
(require 'org-window-habit-config)

;; Forward declarations
(declare-function org-window-habit-property "org-window-habit")


;;; Logbook parsing

(defun org-window-habit-logbook-drawer-bounds ()
  "Return the bounds (start end) of the LOGBOOK drawer at point, or nil."
  (when (re-search-forward org-logbook-drawer-re nil t)
    (list (match-beginning 0) (match-end 0))))

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

(defun org-window-habit-parse-only-days (str)
  "Parse ONLY_DAYS property string STR into a list of day symbols.
STR should be a Lisp list like (:monday :wednesday :friday)."
  (when str
    (car (read-from-string str))))


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


;;; Iterator methods

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

(cl-defmethod org-window-habit-effective-start ((iterator org-window-habit-iterator))
  "Return the effective start time for ITERATOR's window."
  (org-window-habit-time-max (oref (oref iterator window) start-time)
                             (oref (oref (oref iterator window-spec) habit) start-time)))


;;; Array scanning

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


;;; Completion counting

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


;;; Conforming ratio computation

(cl-defmethod org-window-habit-conforming-ratio
  ((iterator org-window-habit-iterator) &rest args)
  "Calculate the conforming ratio for ITERATOR's current window.
ARGS are passed to the completion counting function.
Returns completions / (scale * target * baseline), clamped to [0.0, max-ratio].
A ratio of 1.0 means fully conforming; lower values indicate falling behind.
Values above 1.0 represent extra credit when max-conforming-ratio > 1.0."
  (with-slots (window-spec window start-index) iterator
    (let ((baseline (oref window-spec conforming-baseline))
          (max-ratio (oref window-spec max-conforming-ratio)))
      (min
       max-ratio
       (/
        (apply #'org-window-habit-get-completion-count
               (oref window-spec habit)
               (oref window start-time)
               (oref window end-time)
               :start-index start-index
               args)
        (* (org-window-habit-actual-window-scale iterator)
           (oref window-spec target-repetitions)
           baseline))))))

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


;;; Next required interval

(cl-defmethod org-window-habit-get-next-required-interval
  ((habit org-window-habit) &optional now)
  "Find the next time when HABIT will need a completion.
Searches forward from NOW until the conforming ratio drops below
reschedule-threshold.  Respects reschedule-days and only-days restrictions."
  (setq now (or now (current-time)))
  (with-slots
      (window-specs reschedule-interval reschedule-threshold assessment-interval
                    aggregation-fn done-times only-days reschedule-days)
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
      ;; Snap to next allowed reschedule day
      (org-window-habit-next-allowed-day
       raw-result
       (org-window-habit-effective-reschedule-days only-days reschedule-days)))))

(cl-defmethod org-window-habit-get-future-required-intervals
  ((habit org-window-habit) count &optional now)
  "Compute COUNT future required intervals for HABIT starting from NOW.
Returns a list of times representing when completions will be required,
assuming minimum-effort completion (completing exactly when required).

Each interval is computed by:
1. Finding when the habit would fall out of conformity
2. Simulating a completion at that time
3. Finding the next required time with the simulated completion
4. Repeating COUNT times

This enables prospective planning: showing future \"must complete by\" dates
assuming you complete at the last possible moment each time."
  (setq now (or now (current-time)))
  (with-slots (window-specs assessment-interval reschedule-interval
                            reschedule-threshold max-repetitions-per-interval
                            aggregation-fn only-days reschedule-days start-time)
      habit
    (let ((result '())
          ;; Copy done-times to a list we can extend with simulated completions
          (simulated-done-times (append (oref habit done-times) nil)))
      (cl-loop
       repeat count
       do
       ;; Create deep copies of window-specs to avoid mutation of original habit
       ;; (initialize-instance sets the habit back-reference on each spec)
       (let* ((copied-specs
               (cl-loop for spec in window-specs
                        collect (make-instance 'org-window-habit-window-spec
                                               :duration (oref spec duration-plist)
                                               :repetitions (oref spec target-repetitions)
                                               :value (oref spec conforming-value)
                                               :find-window (oref spec find-window))))
              (temp-habit (make-instance 'org-window-habit
                                         :window-specs copied-specs
                                         :assessment-interval assessment-interval
                                         :reschedule-interval reschedule-interval
                                         :reschedule-threshold reschedule-threshold
                                         :max-repetitions-per-interval max-repetitions-per-interval
                                         :aggregation-fn aggregation-fn
                                         :only-days only-days
                                         :reschedule-days reschedule-days
                                         :done-times (vconcat
                                                      (sort (copy-sequence simulated-done-times)
                                                            (lambda (a b) (time-less-p b a))))
                                         :start-time start-time))
              (next-required (org-window-habit-get-next-required-interval temp-habit now)))
         ;; Add to results
         (push next-required result)
         ;; Simulate completion at this time for next iteration
         (push next-required simulated-done-times)
         ;; Update now to be after the simulated completion
         (setq now next-required)))
      ;; Return in chronological order
      (nreverse result))))


;;; Assessment functions

(cl-defmethod org-window-habit-assess-interval
  ((habit org-window-habit) iterators &rest args)
  "Compute aggregate conforming value for HABIT using ITERATORS.
ARGS are passed to the conforming value computation."
  (let* ((conforming-values
          (cl-loop for iterator in iterators
                   collect (apply #'org-window-habit-get-conforming-value iterator args))))
    (or (funcall (oref habit aggregation-fn) conforming-values) 0.0)))

(cl-defmethod org-window-habit-get-window-specs-status
  ((habit org-window-habit) &optional time)
  "Get conforming status for each window spec in HABIT at TIME.
TIME defaults to current time.
Returns an alist with:
  - windowSpecsStatus: list of per-spec status with conformingRatio,
    completionsInWindow, targetRepetitions, duration, and conformingValue
  - aggregatedConformingRatio: the overall conforming ratio after aggregation"
  (setq time (or time (current-time)))
  (with-slots (window-specs aggregation-fn) habit
    (let* ((iterators (cl-loop for spec in window-specs
                               collect (org-window-habit-iterator-from-time spec time)))
           (per-spec-data
            (cl-loop for iterator in iterators
                     for spec = (oref iterator window-spec)
                     for window = (oref iterator window)
                     for conforming-ratio = (org-window-habit-conforming-ratio iterator)
                     for start-index = (oref iterator start-index)
                     for end-index = (oref iterator end-index)
                     for completions = (- end-index start-index)
                     collect `(("conformingRatio" . ,conforming-ratio)
                               ("completionsInWindow" . ,completions)
                               ("targetRepetitions" . ,(oref spec target-repetitions))
                               ("duration" . ,(oref spec duration-plist))
                               ("conformingValue" . ,(oref spec conforming-value))
                               ("windowStart" . ,(oref window start-time))
                               ("windowEnd" . ,(oref window end-time)))))
           (conforming-values
            (cl-loop for iterator in iterators
                     collect (org-window-habit-get-conforming-value iterator)))
           (aggregated-ratio (or (funcall aggregation-fn conforming-values) 0.0)))
      `(("windowSpecsStatus" . ,per-spec-data)
        ("aggregatedConformingRatio" . ,aggregated-ratio)))))

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


;;; Instance creation from org entry

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an `org-window-habit' instance from the current org entry.
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

(provide 'org-window-habit-core)
;;; org-window-habit-core.el ends here
