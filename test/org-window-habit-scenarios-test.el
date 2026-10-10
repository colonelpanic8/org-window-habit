;;; org-window-habit-scenarios-test.el --- Scenario-based habit tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenario-based habit tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; ==========================================================================
;;; Scenario-Based Tests: Real-World Habit Configurations
;;; ==========================================================================

;;; ---------------------------------------------------------------------------
;;; Scenario: Weekly Exercise Habit (5x per 7 days)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-weekly-exercise-perfect-week ()
  "Test weekly exercise: exactly 5 completions in 7 days = 100% conforming."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 5 completions spread across the week
         (done-times (list (owh-test-make-time 2024 1 15)  ; Monday
                           (owh-test-make-time 2024 1 13)  ; Saturday
                           (owh-test-make-time 2024 1 12)  ; Friday
                           (owh-test-make-time 2024 1 10)  ; Wednesday
                           (owh-test-make-time 2024 1 9))) ; Tuesday
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 5 completions / 5 required = 1.0
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-weekly-exercise-partial-week ()
  "Test weekly exercise: 3 completions in 7 days.
Due to effective window scaling (habit start = earliest completion),
the actual ratio is 3 / (scale * 5)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Only 3 completions
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 10)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 3 completions, window scale ~0.857, so ratio ~0.7
    (should (< (abs (- ratio 0.7)) 0.05))))

(ert-deftest owh-test-scenario-weekly-exercise-overachiever ()
  "Test weekly exercise: 7 completions caps at 100% (not 140%)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 7 completions - one each day
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)
                           (owh-test-make-time 2024 1 10)
                           (owh-test-make-time 2024 1 9)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Should cap at 1.0, not exceed
    (should (= ratio 1.0))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Medication Reminder (every 8 hours, 3x per day)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-medication-three-times-daily ()
  "Test medication habit: 3 doses per day with hourly assessment."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:hours 24)
                              :repetitions 3))
         ;; Three doses at 8am, 2pm, 10pm
         (done-times (list (owh-test-make-time 2024 1 15 22 0 0)
                           (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)))
         (habit (owh-test-make-habit (list spec) done-times '(:hours 1)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-medication-missed-dose ()
  "Test medication habit: only 2 of 3 doses taken.
Since habit start = earliest completion (8am), effective window is 16 hours.
Required reps scale to 3 * (16/24) = 2, so 2 completions = 100%."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:hours 24)
                              :repetitions 3))
         ;; Only two doses at 8am and 10pm
         (done-times (list (owh-test-make-time 2024 1 15 22 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)))
         (habit (owh-test-make-habit (list spec) done-times '(:hours 1)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; With effective window scaling, 2 completions meets the scaled requirement
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-medication-truly-missed-dose ()
  "Test medication habit where a dose is genuinely missed.
Use explicit start-time to avoid window scaling."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:hours 24)
                              :repetitions 3))
         ;; Only two doses, but habit started at midnight
         (done-times (list (owh-test-make-time 2024 1 15 22 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)))
         ;; Explicitly set start-time to midnight so full window applies
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:hours 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 23 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; With full 24h window: 2/3 = ~0.67
    (should (< (abs (- ratio 0.667)) 0.05))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Monthly Reading Goal (4 books per month)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-monthly-reading-complete ()
  "Test monthly reading: 4 books completed in a month."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:months 1)
                              :repetitions 4))
         ;; 4 books finished throughout the month
         (done-times (list (owh-test-make-time 2024 1 28)
                           (owh-test-make-time 2024 1 21)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 7)))
         (habit (owh-test-make-habit (list spec) done-times '(:days 1)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 30)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    (should (>= ratio 0.99))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Multiple Window Specs (Drink water: 8 glasses/day AND 1 glass/hour)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-multiple-windows-both-satisfied ()
  "Test habit with two window specs where both are satisfied."
  (let* ((daily-spec (make-instance 'org-window-habit-window-spec
                                    :duration '(:days 1)
                                    :repetitions 8
                                    :value 1.0))
         (hourly-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:hours 2)
                                     :repetitions 1
                                     :value 0.5))
         ;; 8 glasses spread through the day
         (done-times (list (owh-test-make-time 2024 1 15 20 0 0)
                           (owh-test-make-time 2024 1 15 18 0 0)
                           (owh-test-make-time 2024 1 15 16 0 0)
                           (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 12 0 0)
                           (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)
                           (owh-test-make-time 2024 1 15 6 0 0)))
         (habit (owh-test-make-habit (list daily-spec hourly-spec)
                                     done-times '(:hours 1)))
         (daily-iterator (org-window-habit-iterator-from-time
                          daily-spec (owh-test-make-time 2024 1 15 21 0 0)))
         (daily-ratio (org-window-habit-conforming-ratio daily-iterator)))
    ;; Daily spec should be fully satisfied
    (should (>= daily-ratio 0.99))))

(ert-deftest owh-test-scenario-multiple-windows-aggregation ()
  "Test that aggregation uses minimum of multiple window conforming values."
  (let* ((strict-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:days 1)
                                     :repetitions 10))  ; Hard to satisfy
         (lenient-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 7)
                                      :repetitions 5))  ; Easy to satisfy
         ;; 5 completions - satisfies lenient but not strict
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)))
         (habit (owh-test-make-habit (list strict-spec lenient-spec) done-times))
         (strict-iter (org-window-habit-iterator-from-time
                       strict-spec (owh-test-make-time 2024 1 15)))
         (lenient-iter (org-window-habit-iterator-from-time
                        lenient-spec (owh-test-make-time 2024 1 15)))
         (strict-ratio (org-window-habit-conforming-ratio strict-iter))
         (lenient-ratio (org-window-habit-conforming-ratio lenient-iter)))
    ;; Lenient should be satisfied (5/5 = 1.0)
    (should (>= lenient-ratio 0.99))
    ;; Strict should be partial (1/10 = 0.1 for today only)
    (should (< strict-ratio 0.5))
    ;; Aggregation (min) should return the strict (lower) value
    (let ((values (list (list strict-ratio nil) (list lenient-ratio nil))))
      (should (< (org-window-habit-default-aggregation-fn values) 0.5)))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Boundary Conditions
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-completion-at-window-boundary ()
  "Test completion exactly at window start boundary."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 1))
         ;; Completion exactly at midnight (window boundary)
         (done-times (list (owh-test-make-time 2024 1 9 0 0 0)))
         (habit (owh-test-make-habit (list spec) done-times))
         ;; Check on Jan 15, window is Jan 9-16
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 12 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Completion at start boundary should count
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-completion-just-outside-window ()
  "Test completion one second before window starts doesn't count."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 1))
         ;; Completion one day before window starts
         (done-times (list (owh-test-make-time 2024 1 8 23 59 59)))
         (habit (owh-test-make-habit (list spec) done-times))
         ;; Window for Jan 15 assessment is Jan 9-16
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 12 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Completion before window should not count
    (should (< ratio 0.01))))

(ert-deftest owh-test-scenario-empty-window-zero-conforming ()
  "Test that empty window gives 0% conforming."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (habit (owh-test-make-habit (list spec) '()))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    (should (= ratio 0.0))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Max Repetitions Per Interval Edge Cases
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-burst-completions-capped ()
  "Test that multiple completions in one interval are capped."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 7))
         ;; 7 completions all on the same day
         (done-times (list (owh-test-make-time 2024 1 15 20 0 0)
                           (owh-test-make-time 2024 1 15 18 0 0)
                           (owh-test-make-time 2024 1 15 16 0 0)
                           (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 12 0 0)
                           (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 15 8 0 0)))
         ;; max-repetitions-per-interval = 1 (default)
         (habit (owh-test-make-habit (list spec) done-times '(:days 1) 1))
         (start (owh-test-make-time 2024 1 9))
         (end (owh-test-make-time 2024 1 16))
         (count (org-window-habit-get-completion-count habit start end)))
    ;; All 7 completions on Jan 15, but max 1 per day, so only 1 counts
    (should (= count 1))))

(ert-deftest owh-test-scenario-high-max-reps-per-interval ()
  "Test habit that allows multiple completions per interval."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 1)
                              :repetitions 8))
         ;; 8 glasses of water in one day
         (done-times (list (owh-test-make-time 2024 1 15 21 0 0)
                           (owh-test-make-time 2024 1 15 19 0 0)
                           (owh-test-make-time 2024 1 15 17 0 0)
                           (owh-test-make-time 2024 1 15 15 0 0)
                           (owh-test-make-time 2024 1 15 13 0 0)
                           (owh-test-make-time 2024 1 15 11 0 0)
                           (owh-test-make-time 2024 1 15 9 0 0)
                           (owh-test-make-time 2024 1 15 7 0 0)))
         ;; Allow all 8 in one day
         (habit (owh-test-make-habit (list spec) done-times '(:days 1) 8))
         (start (owh-test-make-time 2024 1 15))
         (end (owh-test-make-time 2024 1 16))
         (count (org-window-habit-get-completion-count habit start end)))
    ;; All 8 should count
    (should (= count 8))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Iterator Traversal Over Multiple Days
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-iterator-week-traversal ()
  "Test iterator advancing through a full week."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 11)
                           (owh-test-make-time 2024 1 9)
                           (owh-test-make-time 2024 1 7)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 8)))
         (assessment-starts '()))
    ;; Advance 7 times (one week) and collect assessment start times
    (dotimes (_ 7)
      (push (oref (oref iterator window) assessment-start-time) assessment-starts)
      (org-window-habit-advance iterator))
    ;; Should have 7 distinct assessment start times
    (should (= (length assessment-starts) 7))
    ;; Each should be one day apart
    (let ((sorted (sort assessment-starts #'time-less-p)))
      (cl-loop for (t1 t2) on sorted
               while t2
               do (should (owh-test-times-equal-p
                           t2
                           (org-window-habit-keyed-duration-add-plist t1 '(:days 1))))))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Year/Month Boundaries
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-year-boundary-crossing ()
  "Test habit tracking across year boundary (Dec 31 -> Jan 1)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completions spanning year boundary
         (done-times (list (owh-test-make-time 2024 1 2)   ; After new year
                           (owh-test-make-time 2023 12 31) ; New year's eve
                           (owh-test-make-time 2023 12 28)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 3)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; All three completions should count
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-leap-year-february ()
  "Test habit tracking in February of leap year."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 2))
         ;; Completions around Feb 29 (leap day) 2024
         (done-times (list (owh-test-make-time 2024 2 29)  ; Leap day!
                           (owh-test-make-time 2024 2 25)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 3 1)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; Both completions should count
    (should (>= ratio 0.99))))

(ert-deftest owh-test-scenario-month-with-different-lengths ()
  "Test monthly habit across months with different day counts."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:months 1)
                              :repetitions 2))
         ;; Completions in February (28 days in non-leap year)
         (done-times (list (owh-test-make-time 2023 2 15)
                           (owh-test-make-time 2023 2 1)))
         (habit (owh-test-make-habit (list spec) done-times '(:days 1)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2023 2 20)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    (should (>= ratio 0.99))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Sparse Completions Over Long Periods
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-sparse-completions ()
  "Test habit with very sparse completions over many weeks."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 30)
                              :repetitions 10))
         ;; Only 3 completions in 30 days
         (done-times (list (owh-test-make-time 2024 1 30)
                           (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 1)))
         (habit (owh-test-make-habit (list spec) done-times))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 30)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; 3/10 = 0.3
    (should (< (abs (- ratio 0.3)) 0.05))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Very Frequent Habits (multiple times per hour)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-pomodoro-technique ()
  "Test Pomodoro-style habit: 4 sessions per 2 hours."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:hours 2)
                              :repetitions 4))
         ;; 4 pomodoros (25 min each with 5 min break)
         (done-times (list (owh-test-make-time 2024 1 15 11 30 0)  ; 4th
                           (owh-test-make-time 2024 1 15 11 0 0)   ; 3rd
                           (owh-test-make-time 2024 1 15 10 30 0)  ; 2nd
                           (owh-test-make-time 2024 1 15 10 0 0))) ; 1st
         (habit (owh-test-make-habit (list spec) done-times '(:minutes 30) 4))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 15 11 45 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    (should (>= ratio 0.99))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Reschedule Threshold Effects
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-reschedule-threshold-met ()
  "Test next required interval when threshold is met."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Perfect compliance - 5 completions
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :reschedule-interval '(:days 1)
                               :reschedule-threshold 1.0
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :start-time nil))
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 15 12 0 0))))
    ;; Should be scheduled for the future (after Jan 15)
    (should (org-window-habit-time-greater-p
             next-required
             (owh-test-make-time 2024 1 15)))))

(ert-deftest owh-test-default-rescheduling-decouples-weekly-streak ()
  "Daily rescheduling is the default for weekly streak assessment."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:weeks 1 :start :monday)
                              :repetitions 5))
         (monday (owh-test-make-time 2024 1 22 10 0 0))
         (tuesday (owh-test-make-time 2024 1 23 10 0 0))
         (wednesday (owh-test-make-time 2024 1 24 10 0 0))
         (thursday (owh-test-make-time 2024 1 25 10 0 0))
         (friday (owh-test-make-time 2024 1 26 10 0 0))
         (habit (make-instance
                 'org-window-habit
                 :window-specs (list spec)
                 :assessment-interval '(:weeks 1 :start :monday)
                 :max-repetitions-per-interval 5
                 :done-times (vector tuesday monday)
                 :start-time (owh-test-make-time 2024 1 22 0 0 0))))
    (should
     (owh-test-times-equal-p
      (org-window-habit-get-next-required-interval
       habit (owh-test-make-time 2024 1 23 12 0 0))
      (owh-test-make-time 2024 1 24 0 0 0)))
    (oset habit done-times (vector wednesday tuesday monday))
    (should
     (owh-test-times-equal-p
      (org-window-habit-get-next-required-interval
       habit (owh-test-make-time 2024 1 24 12 0 0))
      (owh-test-make-time 2024 1 25 0 0 0)))
    (oset habit done-times (vector friday thursday wednesday tuesday monday))
    (should
     (owh-test-times-equal-p
      (org-window-habit-get-next-required-interval
       habit (owh-test-make-time 2024 1 26 12 0 0))
      (owh-test-make-time 2024 1 29 0 0 0)))
    (should (= (org-window-habit-current-streak
                habit (owh-test-make-time 2024 1 26 12 0 0))
               1))))

(ert-deftest owh-test-reschedule-assessment-interval-explicit-override ()
  "Explicit reschedule-assessment-interval overrides the daily default."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:weeks 1 :start :monday)
                              :repetitions 5))
         (monday (owh-test-make-time 2024 1 22 10 0 0))
         (tuesday (owh-test-make-time 2024 1 23 10 0 0))
         (habit (make-instance
                 'org-window-habit
                 :window-specs (list spec)
                 :assessment-interval '(:weeks 1 :start :monday)
                 :reschedule-assessment-interval
                 '(:weeks 1 :start :monday)
                 :reschedule-interval '(:days 1)
                 :max-repetitions-per-interval 5
                 :done-times (vector tuesday monday)
                 :start-time (owh-test-make-time 2024 1 22 0 0 0))))
    (should
     (owh-test-times-equal-p
      (org-window-habit-get-next-required-interval
       habit (owh-test-make-time 2024 1 23 12 0 0))
      (owh-test-make-time 2024 1 22 0 0 0)))))

;;; ---------------------------------------------------------------------------
;;; Scenario: Complex Multi-Spec Habit (Fitness tracking)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-fitness-multi-spec ()
  "Test fitness habit: 3x strength training per week AND daily stretching."
  (let* ((strength-spec (make-instance 'org-window-habit-window-spec
                                       :duration '(:days 7)
                                       :repetitions 3
                                       :value 1.0))
         (stretch-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 1)
                                      :repetitions 1
                                      :value 0.5))
         ;; Good week: 3 strength + daily stretching
         (done-times (list (owh-test-make-time 2024 1 15 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 15 18 0 0)  ; strength
                           (owh-test-make-time 2024 1 14 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 13 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 13 18 0 0)  ; strength
                           (owh-test-make-time 2024 1 12 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 11 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 11 18 0 0)  ; strength
                           (owh-test-make-time 2024 1 10 7 0 0)   ; stretch
                           (owh-test-make-time 2024 1 9 7 0 0)))  ; stretch
         (habit (owh-test-make-habit (list strength-spec stretch-spec) done-times)))
    ;; Both window specs should be satisfied
    (let* ((strength-iter (org-window-habit-iterator-from-time
                           strength-spec (owh-test-make-time 2024 1 15 20 0 0)))
           (stretch-iter (org-window-habit-iterator-from-time
                          stretch-spec (owh-test-make-time 2024 1 15 20 0 0)))
           (strength-ratio (org-window-habit-conforming-ratio strength-iter))
           (stretch-ratio (org-window-habit-conforming-ratio stretch-iter)))
      (should (>= strength-ratio 0.99))
      (should (>= stretch-ratio 0.99)))))

;;; ---------------------------------------------------------------------------
;;; Scenario: More Multi-Window Spec Edge Cases
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scenario-multi-spec-one-failing ()
  "Test multiple specs where one passes and one fails - aggregation picks minimum."
  (let* ((easy-spec (make-instance 'org-window-habit-window-spec
                                   :duration '(:days 7)
                                   :repetitions 1))   ; Easy: 1 per week
         (hard-spec (make-instance 'org-window-habit-window-spec
                                   :duration '(:days 1)
                                   :repetitions 5))   ; Hard: 5 per day
         ;; Only 1 completion - satisfies easy but not hard
         (done-times (list (owh-test-make-time 2024 1 15 12 0 0)))
         (habit (owh-test-make-habit (list easy-spec hard-spec) done-times))
         (easy-iter (org-window-habit-iterator-from-time
                     easy-spec (owh-test-make-time 2024 1 15 20 0 0)))
         (hard-iter (org-window-habit-iterator-from-time
                     hard-spec (owh-test-make-time 2024 1 15 20 0 0)))
         (easy-ratio (org-window-habit-conforming-ratio easy-iter))
         (hard-ratio (org-window-habit-conforming-ratio hard-iter)))
    ;; Easy spec satisfied
    (should (>= easy-ratio 0.99))
    ;; Hard spec not satisfied (1/5 = 0.2)
    (should (< hard-ratio 0.5))
    ;; Aggregation should use the minimum (hard)
    (let ((agg (org-window-habit-default-aggregation-fn
                (list (list easy-ratio nil) (list hard-ratio nil)))))
      (should (< agg 0.5)))))

(ert-deftest owh-test-scenario-multi-spec-different-durations ()
  "Test specs with very different duration scales (hourly vs monthly)."
  (let* ((hourly-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:hours 4)
                                     :repetitions 2))
         (weekly-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:days 7)
                                     :repetitions 10))
         ;; Completions spread over a week with multiple per day
         (done-times (list (owh-test-make-time 2024 1 15 14 0 0)
                           (owh-test-make-time 2024 1 15 10 0 0)
                           (owh-test-make-time 2024 1 14 14 0 0)
                           (owh-test-make-time 2024 1 14 10 0 0)
                           (owh-test-make-time 2024 1 13 14 0 0)
                           (owh-test-make-time 2024 1 13 10 0 0)
                           (owh-test-make-time 2024 1 12 14 0 0)
                           (owh-test-make-time 2024 1 12 10 0 0)
                           (owh-test-make-time 2024 1 11 14 0 0)
                           (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (owh-test-make-habit (list hourly-spec weekly-spec) done-times '(:hours 1) 2))
         (weekly-iter (org-window-habit-iterator-from-time
                       weekly-spec (owh-test-make-time 2024 1 15 16 0 0)))
         (weekly-ratio (org-window-habit-conforming-ratio weekly-iter)))
    ;; Weekly spec: 10 completions / 10 required (with some scaling)
    (should (>= weekly-ratio 0.9))))

(ert-deftest owh-test-scenario-multi-spec-all-failing ()
  "Test multiple specs where all fail - aggregation picks worst.
Uses explicit start-time to ensure full windows apply."
  (let* ((spec1 (make-instance 'org-window-habit-window-spec
                               :duration '(:days 1)
                               :repetitions 10))
         (spec2 (make-instance 'org-window-habit-window-spec
                               :duration '(:days 7)
                               :repetitions 20))
         ;; Only 1 completion
         (done-times (list (owh-test-make-time 2024 1 15 12 0 0)))
         ;; Explicit start-time to avoid window scaling
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec1 spec2)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :start-time (owh-test-make-time 2024 1 1)))
         (iter1 (org-window-habit-iterator-from-time
                 spec1 (owh-test-make-time 2024 1 15 20 0 0)))
         (iter2 (org-window-habit-iterator-from-time
                 spec2 (owh-test-make-time 2024 1 15 20 0 0)))
         (ratio1 (org-window-habit-conforming-ratio iter1))
         (ratio2 (org-window-habit-conforming-ratio iter2)))
    ;; Both should be low (1/10 and 1/20)
    (should (< ratio1 0.2))
    (should (< ratio2 0.1))
    ;; Aggregation picks minimum
    (let ((agg (org-window-habit-default-aggregation-fn
                (list (list ratio1 nil) (list ratio2 nil)))))
      (should (= agg (min ratio1 ratio2))))))

(ert-deftest owh-test-scenario-multi-spec-with-conforming-values ()
  "Test multiple specs with different conforming-value weights."
  (let* ((primary-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 1)
                                      :repetitions 1
                                      :value 1.0))
         (bonus-spec (make-instance 'org-window-habit-window-spec
                                    :duration '(:days 1)
                                    :repetitions 3
                                    :value 0.2))
         (done-times (list (owh-test-make-time 2024 1 15 12 0 0)))
         (habit (owh-test-make-habit (list primary-spec bonus-spec) done-times))
         (primary-iter (org-window-habit-iterator-from-time
                        primary-spec (owh-test-make-time 2024 1 15 20 0 0)))
         (bonus-iter (org-window-habit-iterator-from-time
                      bonus-spec (owh-test-make-time 2024 1 15 20 0 0))))
    ;; Primary spec should be satisfied
    (should (>= (org-window-habit-conforming-ratio primary-iter) 0.99))
    ;; Bonus spec partial (1/3)
    (should (< (org-window-habit-conforming-ratio bonus-iter) 0.5))))

(ert-deftest owh-test-scenario-three-window-specs ()
  "Test habit with three different window specifications.
Uses explicit start-time to ensure full window durations apply."
  (let* ((daily-spec (make-instance 'org-window-habit-window-spec
                                    :duration '(:days 1)
                                    :repetitions 1))
         (weekly-spec (make-instance 'org-window-habit-window-spec
                                     :duration '(:days 7)
                                     :repetitions 5))
         (monthly-spec (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 30)
                                      :repetitions 20))
         ;; 7 completions over a week
         (done-times (list (owh-test-make-time 2024 1 15)
                           (owh-test-make-time 2024 1 14)
                           (owh-test-make-time 2024 1 13)
                           (owh-test-make-time 2024 1 12)
                           (owh-test-make-time 2024 1 11)
                           (owh-test-make-time 2024 1 10)
                           (owh-test-make-time 2024 1 9)))
         ;; Explicit start-time to ensure full window durations apply
         (habit (make-instance 'org-window-habit
                               :window-specs (list daily-spec weekly-spec monthly-spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :start-time (owh-test-make-time 2023 12 1)))
         (daily-iter (org-window-habit-iterator-from-time
                      daily-spec (owh-test-make-time 2024 1 15 20 0 0)))
         (weekly-iter (org-window-habit-iterator-from-time
                       weekly-spec (owh-test-make-time 2024 1 15 20 0 0)))
         (monthly-iter (org-window-habit-iterator-from-time
                        monthly-spec (owh-test-make-time 2024 1 15 20 0 0))))
    ;; Daily: 1/1 = satisfied
    (should (>= (org-window-habit-conforming-ratio daily-iter) 0.99))
    ;; Weekly: 7/5 = satisfied (capped at 1.0)
    (should (>= (org-window-habit-conforming-ratio weekly-iter) 0.99))
    ;; Monthly: 7/20 = partial (~0.35)
    (should (< (org-window-habit-conforming-ratio monthly-iter) 0.5))))

;;; ---------------------------------------------------------------------------
;;; Multiple Completions Per Day Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-multiple-completions-same-day-default ()
  "Test that multiple completions on the same day count as 1 by default.
This is the max-repetitions-per-interval behavior with default value of 1."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 5 completions all on the same day (Jan 15)
         (done-times (vector
                      (owh-test-make-time 2024 1 15 17 0 0)
                      (owh-test-make-time 2024 1 15 14 0 0)
                      (owh-test-make-time 2024 1 15 11 0 0)
                      (owh-test-make-time 2024 1 15 9 0 0)
                      (owh-test-make-time 2024 1 15 7 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :start-time (owh-test-make-time 2024 1 10 0 0 0))))
    ;; Only 1 completion should count, not 5
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 10 0 0 0)
                  (owh-test-make-time 2024 1 17 0 0 0))))
      (should (= count 1)))))

(ert-deftest owh-test-multiple-completions-different-days ()
  "Test that completions on different days each count."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 5 completions on 5 different days
         (done-times (vector
                      (owh-test-make-time 2024 1 15 10 0 0)
                      (owh-test-make-time 2024 1 14 10 0 0)
                      (owh-test-make-time 2024 1 13 10 0 0)
                      (owh-test-make-time 2024 1 12 10 0 0)
                      (owh-test-make-time 2024 1 11 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :start-time (owh-test-make-time 2024 1 10 0 0 0))))
    ;; All 5 should count
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 10 0 0 0)
                  (owh-test-make-time 2024 1 17 0 0 0))))
      (should (= count 5)))))

(ert-deftest owh-test-multiple-completions-mixed ()
  "Test mixed scenario: some days have multiple, some have one."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 3 completions on Jan 15, 2 on Jan 14, 1 on Jan 13 = 6 total
         ;; But with max-1-per-interval, should count as 3 days
         (done-times (vector
                      (owh-test-make-time 2024 1 15 17 0 0)
                      (owh-test-make-time 2024 1 15 12 0 0)
                      (owh-test-make-time 2024 1 15 8 0 0)
                      (owh-test-make-time 2024 1 14 15 0 0)
                      (owh-test-make-time 2024 1 14 9 0 0)
                      (owh-test-make-time 2024 1 13 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :start-time (owh-test-make-time 2024 1 10 0 0 0))))
    ;; Should count as 3 (one per day for 3 days)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 10 0 0 0)
                  (owh-test-make-time 2024 1 17 0 0 0))))
      (should (= count 3)))))

(ert-deftest owh-test-max-reps-per-interval-higher-value ()
  "Test with max-repetitions-per-interval set to 2."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 10))
         ;; 4 completions on Jan 15, should count as 2 with max=2
         (done-times (vector
                      (owh-test-make-time 2024 1 15 18 0 0)
                      (owh-test-make-time 2024 1 15 14 0 0)
                      (owh-test-make-time 2024 1 15 10 0 0)
                      (owh-test-make-time 2024 1 15 8 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 2
                               :done-times done-times
                               :start-time (owh-test-make-time 2024 1 10 0 0 0))))
    ;; Should count as 2 (max per day is 2)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 10 0 0 0)
                  (owh-test-make-time 2024 1 17 0 0 0))))
      (should (= count 2)))))

(provide 'org-window-habit-scenarios-test)
;;; org-window-habit-scenarios-test.el ends here
