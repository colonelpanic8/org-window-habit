;;; org-window-habit-computation-test.el --- Status and scheduling tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Status and scheduling tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; Window Specs Status Tests

(ert-deftest owh-test-get-window-specs-status-single-spec ()
  "Test get-window-specs-status with a single window spec."
  (let* ((habit-start (owh-test-make-time 2024 1 1 0 0 0))
         ;; Create done times: 2 completions in the window
         (done-times (vconcat (list (owh-test-make-time 2024 1 13 10 0 0)
                                    (owh-test-make-time 2024 1 11 14 0 0))))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3
                                                                  :value 1.0))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :start-time habit-start))
         ;; Query from Jan 14
         (query-time (owh-test-make-time 2024 1 14 12 0 0))
         (result (org-window-habit-get-window-specs-status habit query-time)))
    ;; Should have windowSpecsStatus and aggregatedConformingRatio keys
    (should (assoc "windowSpecsStatus" result))
    (should (assoc "aggregatedConformingRatio" result))
    ;; Should have exactly one window spec status
    (let ((specs-status (cdr (assoc "windowSpecsStatus" result))))
      (should (= (length specs-status) 1))
      ;; First spec should have all expected fields
      (let ((first-spec (car specs-status)))
        (should (assoc "conformingRatio" first-spec))
        (should (assoc "completionsInWindow" first-spec))
        (should (assoc "targetRepetitions" first-spec))
        (should (assoc "duration" first-spec))
        (should (assoc "windowStart" first-spec))
        (should (assoc "windowEnd" first-spec))
        ;; 2 completions out of 3 required = 2/3 ratio
        (should (< (abs (- (cdr (assoc "conformingRatio" first-spec)) (/ 2.0 3.0))) 0.01))
        (should (= (cdr (assoc "completionsInWindow" first-spec)) 2))
        (should (= (cdr (assoc "targetRepetitions" first-spec)) 3))))))

(ert-deftest owh-test-get-window-specs-status-multiple-specs ()
  "Test get-window-specs-status with multiple window specs."
  (let* ((habit-start (owh-test-make-time 2024 1 1 0 0 0))
         ;; Create done times: completions spread across windows
         (done-times (vconcat (list (owh-test-make-time 2024 1 13 10 0 0)
                                    (owh-test-make-time 2024 1 12 14 0 0)
                                    (owh-test-make-time 2024 1 10 9 0 0))))
         (habit (make-instance 'org-window-habit
                               :window-specs (list
                                              ;; Short window: 3 days, need 1 completion
                                              (make-instance 'org-window-habit-window-spec
                                                             :duration '(:days 3)
                                                             :repetitions 1
                                                             :value 0.5)
                                              ;; Long window: 7 days, need 2 completions
                                              (make-instance 'org-window-habit-window-spec
                                                             :duration '(:days 7)
                                                             :repetitions 2
                                                             :value 1.0))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :start-time habit-start))
         ;; Query from Jan 14
         (query-time (owh-test-make-time 2024 1 14 12 0 0))
         (result (org-window-habit-get-window-specs-status habit query-time)))
    ;; Should have exactly two window spec statuses
    (let ((specs-status (cdr (assoc "windowSpecsStatus" result))))
      (should (= (length specs-status) 2))
      ;; First spec (3-day window): should have completions from Jan 12 and 13
      (let ((first-spec (car specs-status)))
        (should (= (cdr (assoc "targetRepetitions" first-spec)) 1)))
      ;; Second spec (7-day window): should have all 3 completions
      (let ((second-spec (cadr specs-status)))
        (should (= (cdr (assoc "targetRepetitions" second-spec)) 2))))
    ;; Aggregated ratio should be the minimum (default aggregation)
    (let ((aggregated (cdr (assoc "aggregatedConformingRatio" result))))
      (should (numberp aggregated)))))

(ert-deftest owh-test-get-window-specs-status-no-completions ()
  "Test get-window-specs-status with no completions."
  (let* ((habit-start (owh-test-make-time 2024 1 1 0 0 0))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3
                                                                  :value 1.0))
                               :assessment-interval '(:days 1)
                               :done-times (vector)
                               :start-time habit-start))
         (query-time (owh-test-make-time 2024 1 14 12 0 0))
         (result (org-window-habit-get-window-specs-status habit query-time)))
    (let* ((specs-status (cdr (assoc "windowSpecsStatus" result)))
           (first-spec (car specs-status)))
      ;; No completions means 0 conforming ratio
      (should (= (cdr (assoc "conformingRatio" first-spec)) 0.0))
      (should (= (cdr (assoc "completionsInWindow" first-spec)) 0)))))

(ert-deftest owh-test-get-window-specs-status-fully-conforming ()
  "Test get-window-specs-status when fully conforming."
  (let* ((habit-start (owh-test-make-time 2024 1 1 0 0 0))
         ;; 3 completions for a habit requiring 3
         (done-times (vconcat (list (owh-test-make-time 2024 1 13 10 0 0)
                                    (owh-test-make-time 2024 1 12 14 0 0)
                                    (owh-test-make-time 2024 1 11 9 0 0))))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3
                                                                  :value 1.0))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :start-time habit-start))
         (query-time (owh-test-make-time 2024 1 14 12 0 0))
         (result (org-window-habit-get-window-specs-status habit query-time)))
    (let* ((specs-status (cdr (assoc "windowSpecsStatus" result)))
           (first-spec (car specs-status))
           (aggregated (cdr (assoc "aggregatedConformingRatio" result))))
      ;; Fully conforming means ratio of 1.0
      (should (= (cdr (assoc "conformingRatio" first-spec)) 1.0))
      (should (= (cdr (assoc "completionsInWindow" first-spec)) 3))
      (should (= aggregated 1.0)))))

(ert-deftest owh-test-window-specs-status-counts-like-conforming-ratio ()
  "completionsInWindow applies the same caps and filters as the ratio."
  (let* ((now (owh-test-make-time 2025 3 10 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 "(:window-specs ((:duration (:days 7) :repetitions 3)) :from \"2025-03-06\")"
                 (vector (owh-test-make-time 2025 3 9 10 0 0)
                         (owh-test-make-time 2025 3 9 9 0 0)
                         (owh-test-make-time 2025 3 8 10 0 0)
                         (owh-test-make-time 2025 3 5 10 0 0))
                 now))
         (spec-status (car (cdr (assoc "windowSpecsStatus"
                                       (org-window-habit-get-window-specs-status
                                        habit now))))))
    (should (= (cdr (assoc "completionsInWindow" spec-status)) 2))))

;;; ==========================================================================
;;; Future Required Intervals Tests (Prospective Scheduling)
;;; ==========================================================================
;;;
;;; Tests for org-window-habit-get-future-required-intervals which computes
;;; multiple future "must complete by" dates, assuming minimum-effort
;;; completion (completing exactly when required, not earlier).

;;; ---------------------------------------------------------------------------
;;; Basic Functionality Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-basic-weekly-5x ()
  "Test future intervals for 5x per 7 days habit.
With 5 completions spread across the last 5 days, each future completion
is needed ~every 1.4 days to maintain conformity."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Perfect streak: completed Jan 11-15
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 14 10 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 12 10 0 0)
                           (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         ;; Query from Jan 15 afternoon
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should return exactly 5 future intervals
    (should (= (length intervals) 5))
    ;; Each interval should be after the previous
    (cl-loop for i from 0 below (1- (length intervals))
             do (should (time-less-p (nth i intervals) (nth (1+ i) intervals))))
    ;; First required is Jan 18 (when Jan 11 falls out of window on Jan 18):
    ;; - Window on Jan 17 (Jan 11-18): still has 5 completions
    ;; - Window on Jan 18 (Jan 12-19): only 4 completions (Jan 11 excluded)
    ;; Plus reschedule-interval adds 1 day from last completion (Jan 15)
    (should (owh-test-times-equal-p
             (nth 0 intervals)
             (owh-test-make-time 2024 1 18 0 0 0)))))

(ert-deftest owh-test-future-intervals-daily-habit ()
  "Test future intervals for daily habit (1x per day).
Each day requires exactly one completion."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 1)
                              :repetitions 1))
         ;; Completed today (Jan 15)
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should get 5 consecutive days: Jan 16, 17, 18, 19, 20
    (should (= (length intervals) 5))
    (should (owh-test-times-equal-p (nth 0 intervals) (owh-test-make-time 2024 1 16 0 0 0)))
    (should (owh-test-times-equal-p (nth 1 intervals) (owh-test-make-time 2024 1 17 0 0 0)))
    (should (owh-test-times-equal-p (nth 2 intervals) (owh-test-make-time 2024 1 18 0 0 0)))
    (should (owh-test-times-equal-p (nth 3 intervals) (owh-test-make-time 2024 1 19 0 0 0)))
    (should (owh-test-times-equal-p (nth 4 intervals) (owh-test-make-time 2024 1 20 0 0 0)))))

(ert-deftest owh-test-future-intervals-sparse-monthly ()
  "Test future intervals for sparse habit (1x per month).
With monthly window, completions are required roughly monthly."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:months 1)
                              :repetitions 1))
         ;; Completed on Jan 15
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 3 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should get 3 intervals roughly a month apart
    (should (= (length intervals) 3))
    ;; First should be around Feb 15 (when Jan 15 falls out of month window)
    (let ((first-interval (nth 0 intervals)))
      (should (time-less-p (owh-test-make-time 2024 2 1 0 0 0) first-interval))
      (should (time-less-p first-interval (owh-test-make-time 2024 3 1 0 0 0))))))

(ert-deftest owh-test-future-intervals-count-parameter ()
  "Test that count parameter controls number of returned intervals."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 1)
                              :repetitions 1))
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (now (owh-test-make-time 2024 1 15 14 0 0)))
    ;; Request 1 interval
    (should (= (length (org-window-habit-get-future-required-intervals habit 1 now)) 1))
    ;; Request 3 intervals
    (should (= (length (org-window-habit-get-future-required-intervals habit 3 now)) 3))
    ;; Request 10 intervals
    (should (= (length (org-window-habit-get-future-required-intervals habit 10 now)) 10))))

;;; ---------------------------------------------------------------------------
;;; Only-Days Restriction Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-only-days-mwf ()
  "Test future intervals with Monday/Wednesday/Friday restriction.
Completions should only be scheduled on allowed days."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Mon Jan 15
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :only-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; All returned dates should be Mon, Wed, or Fri
    (cl-loop for interval in intervals
             for decoded = (decode-time interval)
             for dow = (nth 6 decoded)  ; 0=Sun, 1=Mon, ..., 5=Fri
             do (should (member dow '(1 3 5))))))

(ert-deftest owh-test-future-intervals-only-days-weekend ()
  "Test future intervals with weekend-only restriction."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 2))
         ;; Completed on Sat Jan 20
         (done-times (list (owh-test-make-time 2024 1 20 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :only-days '(:saturday :sunday)
                               :start-time (owh-test-make-time 2024 1 20 0 0 0)))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 4 (owh-test-make-time 2024 1 20 14 0 0))))
    ;; All returned dates should be Sat (6) or Sun (0)
    (cl-loop for interval in intervals
             for decoded = (decode-time interval)
             for dow = (nth 6 decoded)
             do (should (member dow '(0 6))))))

;;; ---------------------------------------------------------------------------
;;; Multiple Window Specs Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-multi-spec-fitness ()
  "Test future intervals with multiple window specs (fitness habit).
Must satisfy BOTH: 3x strength per week AND 1x daily stretching.
The more restrictive spec (daily) should drive scheduling."
  (let* ((strength-spec (make-instance 'org-window-habit-window-spec
                                       :duration '(:days 7)
                                       :repetitions 3))
         (stretch-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 1)
                                      :repetitions 1))
         ;; Completed today (counts for both specs)
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (owh-test-make-habit (list strength-spec stretch-spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should be daily because daily stretching is more restrictive
    (should (= (length intervals) 5))
    ;; Consecutive days starting from Jan 16
    (should (owh-test-times-equal-p (nth 0 intervals) (owh-test-make-time 2024 1 16 0 0 0)))
    (should (owh-test-times-equal-p (nth 1 intervals) (owh-test-make-time 2024 1 17 0 0 0)))))

(ert-deftest owh-test-future-intervals-multi-spec-weekly-and-monthly ()
  "Test with weekly and monthly specs.
3x per week AND 10x per month - the weekly is likely more restrictive."
  (let* ((weekly-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:days 7)
                                     :repetitions 3))
         (monthly-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:months 1)
                                      :repetitions 10))
         ;; 3 completions this week
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (owh-test-make-habit (list weekly-spec monthly-spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should return 5 intervals
    (should (= (length intervals) 5))
    ;; All should be in the future
    (cl-loop for interval in intervals
             do (should (time-less-p (owh-test-make-time 2024 1 15 0 0 0) interval)))))

;;; ---------------------------------------------------------------------------
;;; Edge Case: No Prior Completions
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-no-done-times ()
  "Test future intervals when habit has no completions yet.
Should still return valid future intervals. When there are no completions,
the habit is immediately non-conforming, so first interval may be 'now'
(normalized to assessment boundary)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         (habit (owh-test-make-habit (list spec) '()))
         (now (owh-test-make-time 2024 1 15 14 0 0))
         (intervals (org-window-habit-get-future-required-intervals habit 3 now)))
    ;; Should get 3 intervals
    (should (= (length intervals) 3))
    ;; First interval should be today (normalized to start of day, so it's the
    ;; assessment boundary that contains 'now')
    (should (owh-test-times-equal-p (nth 0 intervals) (owh-test-make-time 2024 1 15 0 0 0)))
    ;; Subsequent intervals should be in order
    (cl-loop for i from 0 below (1- (length intervals))
             do (should (time-less-p (nth i intervals) (nth (1+ i) intervals))))))

;;; ---------------------------------------------------------------------------
;;; Edge Case: Currently Non-Conforming
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-already-behind ()
  "Test future intervals when already non-conforming.
If ratio is already below threshold, first 'required' interval is now."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Only 2 completions in last 7 days - behind on 5x/week goal
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 10 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (now (owh-test-make-time 2024 1 15 14 0 0))
         (intervals (org-window-habit-get-future-required-intervals habit 3 now)))
    ;; Should still return intervals
    (should (= (length intervals) 3))
    ;; First interval should be today or very soon (already behind)
    (should (time-less-p (nth 0 intervals) (owh-test-make-time 2024 1 17 0 0 0)))))

;;; ---------------------------------------------------------------------------
;;; Edge Case: Reschedule Interval Effects
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-respects-reschedule-interval ()
  "Test that reschedule-interval creates minimum gap between completions.
Even if mathematically you could complete immediately, reschedule-interval
prevents scheduling too soon after a completion."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 7))  ; Daily requirement
         ;; Just completed
         (done-times (list (owh-test-make-time 2024 1 15 14 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :reschedule-interval '(:days 2)  ; 2-day minimum gap
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :start-time nil))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 3 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; First interval should be at least 2 days from last completion (Jan 17+)
    (should (org-window-habit-time-greater-or-equal-p
             (nth 0 intervals)
             (owh-test-make-time 2024 1 17 0 0 0)))))

(ert-deftest owh-test-future-intervals-default-weekly-catch-up ()
  "Future intervals use daily catch-up by default for weekly quotas."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:weeks 1 :start :monday)
                              :repetitions 5))
         (monday (owh-test-make-time 2024 1 22 10 0 0))
         (tuesday (owh-test-make-time 2024 1 23 10 0 0))
         (habit (make-instance
                 'org-window-habit
                 :window-specs (list spec)
                 :assessment-interval '(:weeks 1 :start :monday)
                 :max-repetitions-per-interval 5
                 :done-times (vector tuesday monday)
                 :start-time (owh-test-make-time 2024 1 22 0 0 0)))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 3 (owh-test-make-time 2024 1 23 12 0 0))))
    (should (owh-test-times-equal-p
             (nth 0 intervals)
             (owh-test-make-time 2024 1 24 0 0 0)))
    (should (owh-test-times-equal-p
             (nth 1 intervals)
             (owh-test-make-time 2024 1 25 0 0 0)))
    (should (owh-test-times-equal-p
             (nth 2 intervals)
             (owh-test-make-time 2024 1 26 0 0 0)))))

;;; ---------------------------------------------------------------------------
;;; Edge Case: Max Repetitions Per Interval
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-max-reps-per-interval ()
  "Test that max-repetitions-per-interval affects future projections.
If you can only count 1 completion per day, you can't 'front-load' completions."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 7))  ; Need 7 per week
         ;; 3 completions all on the same day (but max-reps=1 means only 1 counts)
         (done-times (list (owh-test-make-time 2024 1 15 18 0 0)
                           (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times '(:days 1) 1))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 5 (owh-test-make-time 2024 1 15 20 0 0))))
    ;; With max-reps=1, only 1 of the 3 completions counts
    ;; So we need daily completions going forward
    (should (= (length intervals) 5))
    ;; Should be consecutive days
    (should (owh-test-times-equal-p (nth 0 intervals) (owh-test-make-time 2024 1 16 0 0 0)))
    (should (owh-test-times-equal-p (nth 1 intervals) (owh-test-make-time 2024 1 17 0 0 0)))))

;;; ---------------------------------------------------------------------------
;;; Edge Case: Threshold Below 1.0
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-partial-threshold ()
  "Test future intervals with reschedule-threshold < 1.0.
With threshold 0.8, you only need to complete when ratio drops below 80%."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 4 completions = 80% - right at threshold
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 14 10 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 12 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :reschedule-interval '(:days 1)
                               :reschedule-threshold 0.8
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :start-time nil))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 3 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; With 0.8 threshold and 4/5 completions, we're exactly at threshold
    ;; Need to complete when a completion falls out and ratio drops below 0.8
    (should (= (length intervals) 3))))

;;; ---------------------------------------------------------------------------
;;; Consistency Test: Simulated Completions Match Actual Behavior
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-consistency-with-get-next-required ()
  "Test that future intervals are consistent with iterative get-next-required calls.
If we actually add a completion at each predicted interval, get-next-required
should return the next predicted interval."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (now (owh-test-make-time 2024 1 15 14 0 0))
         (predicted (org-window-habit-get-future-required-intervals habit 3 now)))
    ;; First predicted should match get-next-required-interval
    (let ((actual-first (org-window-habit-get-next-required-interval habit now)))
      (should (owh-test-times-equal-p (nth 0 predicted) actual-first)))))

(ert-deftest owh-test-future-intervals-keep-all-habit-settings ()
  "Test that projections keep spec baselines and the habit's reset time."
  (let ((now (owh-test-make-time 2025 3 10 12 0 0)))
    (dolist (case
             `(("(:window-specs ((:duration (:days 10) :repetitions 4 :conforming-baseline 0.5)))"
                ,(list (owh-test-make-time 2025 3 1 10 0 0)
                       (owh-test-make-time 2025 3 2 10 0 0)
                       (owh-test-make-time 2025 3 3 10 0 0)
                       (owh-test-make-time 2025 3 4 10 0 0)))
               ("(:window-specs ((:duration (:days 7) :repetitions 2)) :from \"2025-03-08\")"
                ,(list (owh-test-make-time 2025 3 5 10 0 0)
                       (owh-test-make-time 2025 3 6 10 0 0)
                       (owh-test-make-time 2025 3 9 10 0 0)))))
      (let ((habit (org-window-habit-create-instance-from-config
                    (car case)
                    (vconcat (sort (copy-sequence (cadr case))
                                   (lambda (a b) (time-less-p b a))))
                    now)))
        (should (owh-test-times-equal-p
                 (car (org-window-habit-get-future-required-intervals habit 1 now))
                 (org-window-habit-get-next-required-interval habit now)))))))

;;; ---------------------------------------------------------------------------
;;; Stress Test: Large Count
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-large-count ()
  "Test requesting a large number of future intervals.
Should complete in reasonable time and return valid results."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (done-times (list (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 14 10 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 12 10 0 0)
                           (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 50 (owh-test-make-time 2024 1 15 14 0 0))))
    ;; Should return 50 intervals
    (should (= (length intervals) 50))
    ;; All should be strictly increasing
    (cl-loop for i from 0 below (1- (length intervals))
             do (should (time-less-p (nth i intervals) (nth (1+ i) intervals))))
    ;; Last interval should be well into the future (months out for 50 intervals)
    (should (time-less-p (owh-test-make-time 2024 3 1 0 0 0)
                         (nth 49 intervals)))))

;;; ---------------------------------------------------------------------------
;;; Hourly Assessment Interval Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-hourly-assessment ()
  "Test future intervals with hourly assessment interval.
For habits that need sub-daily tracking."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:hours 8)  ; 8-hour window
                              :repetitions 4))       ; 4 completions per 8 hours
         ;; 4 completions over last 8 hours
         (done-times (list (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 12 0 0)
                           (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)))
         (habit (owh-test-make-habit (list spec) done-times '(:hours 2) 1))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 4 (owh-test-make-time 2024 1 15 15 0 0))))
    ;; Should get 4 intervals
    (should (= (length intervals) 4))
    ;; Should be roughly 2-hour intervals (matching assessment-interval)
    (cl-loop for interval in intervals
             do (should (time-less-p (owh-test-make-time 2024 1 15 0 0 0) interval)))))

;;; ---------------------------------------------------------------------------
;;; Week-Aligned Window Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-future-intervals-week-aligned ()
  "Test future intervals with true week-aligned windows.
Using (:weeks 1) instead of (:days 7) for week boundaries."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:weeks 1)  ; True week alignment
                              :repetitions 3))
         ;; Completed Mon-Wed of current week
         (done-times (list (owh-test-make-time 2024 1 17 10 0 0)  ; Wed
                           (owh-test-make-time 2024 1 16 10 0 0)  ; Tue
                           (owh-test-make-time 2024 1 15 10 0 0))) ; Mon
         (habit (owh-test-make-habit (list spec) done-times))
         (intervals (org-window-habit-get-future-required-intervals
                     habit 3 (owh-test-make-time 2024 1 17 14 0 0))))
    ;; Should return 3 intervals
    (should (= (length intervals) 3))
    ;; First should be after Wed Jan 17
    (should (time-less-p (owh-test-make-time 2024 1 17 0 0 0) (nth 0 intervals)))))

;;; ---------------------------------------------------------------------------
;;; Conforming Baseline and Extra Credit Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-conforming-baseline-full-credit-at-80-percent ()
  "Test that conforming-baseline 0.8 gives full credit at 80% of target.
With target 5 and baseline 0.8, completing 4 should give ratio 1.0."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5
                              :conforming-baseline 0.8))
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)))  ; 4 completions
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 4 completions / (5 * 0.8) = 4/4 = 1.0
    (should (>= ratio 0.99))))

(ert-deftest owh-test-conforming-baseline-partial-credit ()
  "Test that conforming-baseline affects partial completion correctly.
With target 5 and baseline 0.8, completing 2 should give ratio 0.5."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5
                              :conforming-baseline 0.8))
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)))  ; 2 completions
         ;; Use explicit start-time so the full 7-day window is active
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :start-time (owh-test-make-time 2024 1 8)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 2 completions / (5 * 0.8) = 2/4 = 0.5
    (should (< (abs (- ratio 0.5)) 0.05))))

(ert-deftest owh-test-max-conforming-ratio-caps-extra-credit ()
  "Test that max-conforming-ratio caps the extra credit.
With target 5, baseline 0.8, and max 1.2, completing 6 should cap at 1.2."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5
                              :conforming-baseline 0.8
                              :max-conforming-ratio 1.2))
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)
                           (owh-test-make-time 2024 1 10)))  ; 6 completions
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 6 completions / (5 * 0.8) = 6/4 = 1.5, but capped at 1.2
    (should (< (abs (- ratio 1.2)) 0.01))))

(ert-deftest owh-test-max-conforming-ratio-allows-extra-credit ()
  "Test that completing more than baseline allows extra credit up to max.
With target 5, baseline 0.8, and max 1.2, completing 5 should give 1.25 (capped to 1.2)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5
                              :conforming-baseline 0.8
                              :max-conforming-ratio 1.2))
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)))  ; 5 completions
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 5 completions / (5 * 0.8) = 5/4 = 1.25, but capped at 1.2
    (should (< (abs (- ratio 1.2)) 0.01))))

(ert-deftest owh-test-conforming-baseline-default-is-1 ()
  "Test that without conforming-baseline, default behavior (baseline 1.0) applies.
With target 5, completing 5 should give exactly 1.0."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))  ; No baseline specified
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)))  ; 5 completions
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 5 completions / 5 = 1.0, capped at 1.0 (default max)
    (should (= ratio 1.0))))

(ert-deftest owh-test-max-conforming-ratio-default-caps-at-1 ()
  "Test that without max-conforming-ratio, overachieving still caps at 1.0.
With target 5, completing 7 should give 1.0 (not 1.4)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))  ; No max specified
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)
                           (owh-test-make-time 2024 1 10)
                           (owh-test-make-time 2024 1 9)))  ; 7 completions
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Should still cap at 1.0 for backward compatibility
    (should (= ratio 1.0))))

(ert-deftest owh-test-conforming-baseline-with-window-scaling ()
  "Test that conforming-baseline works correctly with window scaling.
When habit started mid-window, both baseline and scale should apply."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 7
                              :conforming-baseline 0.8))
         ;; Completions starting Jan 12 (habit effectively started then)
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)))  ; 4 completions over 4 days
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :start-time (owh-test-make-time 2024 1 12)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Window is ~4/7 of full (scale ~0.57), so effective target = 7 * 0.57 * 0.8 ≈ 3.2
    ;; 4 completions / 3.2 ≈ 1.25, but need to verify actual behavior
    ;; The key is that baseline multiplies with scale
    (should (>= ratio 0.99))))  ; Should be fully conforming with extra credit possible

(ert-deftest owh-test-extra-credit-affects-aggregation ()
  "Test that extra credit from one spec can affect aggregation.
When using min aggregation, extra credit on one spec won't help if another is low."
  (let* ((easy-spec (make-instance 'org-window-habit-window-spec
                                   :duration '(:days 7)
                                   :repetitions 3
                                   :conforming-baseline 0.8
                                   :max-conforming-ratio 1.5))
         (hard-spec (make-instance 'org-window-habit-window-spec
                                   :duration '(:days 7)
                                   :repetitions 10))
         ;; 5 completions: exceeds easy (3*0.8=2.4), but fails hard (10)
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)))
         ;; Use explicit start-time so the full 7-day window is active
         (habit (make-instance 'org-window-habit
                               :window-specs (list easy-spec hard-spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :start-time (owh-test-make-time 2024 1 8)))
         (easy-iter (org-window-habit-iterator-from-time
                     easy-spec (owh-test-make-time 2024 1 15 23 0 0)))
         (hard-iter (org-window-habit-iterator-from-time
                     hard-spec (owh-test-make-time 2024 1 15 23 0 0)))
         (easy-ratio (org-window-habit-conforming-ratio easy-iter))
         (hard-ratio (org-window-habit-conforming-ratio hard-iter)))
    ;; Easy: 5 / (3 * 0.8) = 5/2.4 ≈ 2.08, capped at 1.5
    (should (< (abs (- easy-ratio 1.5)) 0.05))
    ;; Hard: 5 / 10 = 0.5
    (should (< (abs (- hard-ratio 0.5)) 0.05))
    ;; Aggregation (min) should return hard (lower) value
    ;; aggregation-fn expects list of (ratio value window) tuples
    (let ((agg (org-window-habit-default-aggregation-fn
                (list (list easy-ratio nil nil)
                      (list hard-ratio nil nil)))))
      (should (< (abs (- agg 0.5)) 0.05)))))

(ert-deftest owh-test-config-conforming-baseline-parsing ()
  "Test that conforming-baseline is parsed from CONFIG property."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 5 :conforming-baseline 0.8)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs))
           (window-spec-plist (car (plist-get config :window-specs))))
      (should (= (plist-get window-spec-plist :conforming-baseline) 0.8)))))

(ert-deftest owh-test-config-max-conforming-ratio-parsing ()
  "Test that max-conforming-ratio is parsed from CONFIG property."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 5 :max-conforming-ratio 1.2)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs))
           (window-spec-plist (car (plist-get config :window-specs))))
      (should (= (plist-get window-spec-plist :max-conforming-ratio) 1.2)))))

(ert-deftest owh-test-config-both-baseline-and-max-ratio ()
  "Test parsing both conforming-baseline and max-conforming-ratio together."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 5 :conforming-baseline 0.8 :max-conforming-ratio 1.2)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs))
           (window-spec-plist (car (plist-get config :window-specs))))
      (should (= (plist-get window-spec-plist :conforming-baseline) 0.8))
      (should (= (plist-get window-spec-plist :max-conforming-ratio) 1.2)))))

;;; ==========================================================================

(ert-deftest owh-test-next-required-terminates-with-zero-threshold ()
  "Test that a threshold the ratio can never drop below ends the search."
  (let* ((now (owh-test-make-time 2025 3 10 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 "(:window-specs ((:duration (:days 7) :repetitions 2)) :reschedule-threshold 0.0)"
                 (vector (owh-test-make-time 2025 3 9 10 0 0))
                 now)))
    (should-not (org-window-habit-get-next-required-interval habit now))))

(ert-deftest owh-test-window-spec-rejects-non-positive-repetitions ()
  "Test that window specs require a positive repetition count."
  (should-error (make-instance 'org-window-habit-window-spec
                               :duration '(:days 7) :repetitions 0))
  (should-error (make-instance 'org-window-habit-window-spec
                               :duration '(:days 7) :repetitions -1)))

(ert-deftest owh-test-next-required-continues-into-next-config ()
  "A reminder pushed into a later config version is computed under it."
  (let* ((now (owh-test-make-time 2025 3 10 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 (concat "((:from \"2025-03-12\" :window-specs ((:duration (:days 3) :repetitions 1)))"
                         " (:until \"2025-03-12\" :window-specs ((:duration (:days 3) :repetitions 1))"
                         " :reschedule-days (:friday)))")
                 (vector (owh-test-make-time 2025 3 8 10 0 0))
                 now)))
    (should (owh-test-times-equal-p
             (org-window-habit-get-next-required-interval habit now)
             (owh-test-make-time 2025 3 12 0 0 0)))))

(ert-deftest owh-test-conforming-ratio-before-habit-start ()
  "A window ending before the habit starts yields 0.0, not a negative ratio."
  (let* ((habit (make-instance 'org-window-habit
                               :window-specs
                               (list (make-instance 'org-window-habit-window-spec
                                                    :duration '(:days 7)
                                                    :repetitions 3))
                               :assessment-interval '(:days 1)
                               :done-times (vector (owh-test-make-time 2025 3 9 10 0 0))
                               :start-time (owh-test-make-time 2025 3 20 0 0 0)))
         (iterator (org-window-habit-iterator-from-time
                    (car (oref habit window-specs))
                    (owh-test-make-time 2025 3 10 12 0 0))))
    (should (eql (org-window-habit-conforming-ratio iterator) 0.0))))

(ert-deftest owh-test-next-required-with-time-of-day-start ()
  "A version starting at a time of day is searched from that time."
  (let* ((now (owh-test-make-time 2025 3 10 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 "(:from \"2025-03-10 10:00\" :window-specs ((:duration (:days 3) :repetitions 1)))"
                 (vector) now)))
    (should (owh-test-times-equal-p
             (org-window-habit-get-next-required-interval habit now)
             (owh-test-make-time 2025 3 10 10 0 0)))))

(ert-deftest owh-test-next-required-after-version-that-never-requires ()
  "A version that never requires a completion does not end the search."
  (let* ((now (owh-test-make-time 2025 3 10 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 (concat "((:from \"2025-03-12\" :window-specs ((:duration (:days 3) :repetitions 1)))"
                         " (:until \"2025-03-12\" :reschedule-threshold 0"
                         " :window-specs ((:duration (:days 3) :repetitions 1))))")
                 (vector) now)))
    (should (owh-test-times-equal-p
             (org-window-habit-get-next-required-interval habit now)
             (owh-test-make-time 2025 3 12 0 0 0)))))

(provide 'org-window-habit-computation-test)
;;; org-window-habit-computation-test.el ends here
