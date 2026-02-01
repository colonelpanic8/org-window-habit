;;; org-window-habit-computation.el --- Iterators, completion counting, assessment -*- lexical-binding: t; -*-

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

;; Iterator, completion counting, conforming ratio, and assessment
;; functions for org-window-habit.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'org-window-habit-time)
(require 'org-window-habit-core)


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

(provide 'org-window-habit-computation)
;;; org-window-habit-computation.el ends here
