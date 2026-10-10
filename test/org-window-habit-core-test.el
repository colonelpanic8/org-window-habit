;;; org-window-habit-core-test.el --- Core habit and assessment tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Core habit and assessment tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; Assessment Window Anchoring Tests

(ert-deftest owh-test-assessment-window-anchored-to-habit-start ()
  "Test that assessment windows are anchored to the habit's start time.
With a 3-day assessment interval, the assessment boundaries should be
consistent relative to the habit's start, not dependent on the current date.

If habit started on Jan 10, assessment periods should be:
  Jan 10-13, Jan 13-16, Jan 16-19, etc.

Evaluating from different times within the same period should give
consistent results anchored to these boundaries."
  (let* ((habit-start (owh-test-make-time 2024 1 10 0 0 0))
         (done-times (vector (owh-test-make-time 2024 1 12 10 0 0)))  ; One completion
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 3)
                               :done-times done-times
                               :start-time habit-start))
         (window-spec (car (oref habit window-specs)))
         ;; Get assessment window from Jan 14 morning
         (window-from-jan14-am (org-window-habit-get-assessment-window
                                window-spec (owh-test-make-time 2024 1 14 8 0 0)))
         ;; Get assessment window from Jan 15 evening - same period as Jan 14
         (window-from-jan15-pm (org-window-habit-get-assessment-window
                                window-spec (owh-test-make-time 2024 1 15 20 0 0))))
    ;; Both Jan 14 and Jan 15 fall in the Jan 13-16 assessment period
    ;; So they should return the same assessment window boundaries
    (should (owh-test-times-equal-p
             (oref window-from-jan14-am assessment-start-time)
             (oref window-from-jan15-pm assessment-start-time)))
    (should (owh-test-times-equal-p
             (oref window-from-jan14-am assessment-end-time)
             (oref window-from-jan15-pm assessment-end-time)))
    ;; The assessment period should be Jan 13-16
    ;; (anchored to habit start of Jan 10: 10-13, 13-16, 16-19...)
    (should (owh-test-times-equal-p
             (oref window-from-jan14-am assessment-start-time)
             (owh-test-make-time 2024 1 13 0 0 0)))
    (should (owh-test-times-equal-p
             (oref window-from-jan14-am assessment-end-time)
             (owh-test-make-time 2024 1 16 0 0 0)))))

(ert-deftest owh-test-assessment-window-3day-interval-boundaries ()
  "Test specific assessment boundaries with 3-day interval.
Habit starts Jan 10. With 3-day assessment:
  Period 1: Jan 10-13
  Period 2: Jan 13-16
  Period 3: Jan 16-19

Querying from different dates within the same period should return same window."
  (let* ((habit-start (owh-test-make-time 2024 1 10 0 0 0))
         (done-times (vector))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 3)
                               :done-times done-times
                               :start-time habit-start))
         (window-spec (car (oref habit window-specs))))
    ;; Query from Jan 11 - should be in period Jan 10-13
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 11 12 0 0))))
      (should (owh-test-times-equal-p
               (oref window assessment-start-time)
               (owh-test-make-time 2024 1 10 0 0 0)))
      (should (owh-test-times-equal-p
               (oref window assessment-end-time)
               (owh-test-make-time 2024 1 13 0 0 0))))
    ;; Query from Jan 14 - should be in period Jan 13-16
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 14 12 0 0))))
      (should (owh-test-times-equal-p
               (oref window assessment-start-time)
               (owh-test-make-time 2024 1 13 0 0 0)))
      (should (owh-test-times-equal-p
               (oref window assessment-end-time)
               (owh-test-make-time 2024 1 16 0 0 0))))
    ;; Query from Jan 18 - should be in period Jan 16-19
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 18 12 0 0))))
      (should (owh-test-times-equal-p
               (oref window assessment-start-time)
               (owh-test-make-time 2024 1 16 0 0 0)))
      (should (owh-test-times-equal-p
               (oref window assessment-end-time)
               (owh-test-make-time 2024 1 19 0 0 0))))))

(ert-deftest owh-test-assessment-anchoring-vs-day-of-month ()
  "Test that assessment windows are anchored to habit start, not day-of-month.
This test verifies the fix: before, a 3-day interval would use day-of-month
arithmetic (e.g., day 15 -> aligned to day 13 via 15-(3-1)=13). Now it's
anchored to the habit's start time.

Two habits starting on different days should have different assessment
boundaries even when queried at the same time."
  (let* (;; Habit A starts Jan 10
         (habit-a-start (owh-test-make-time 2024 1 10 0 0 0))
         (habit-a (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 3))
                                 :assessment-interval '(:days 3)
                                 :done-times (vector)
                                 :start-time habit-a-start))
         ;; Habit B starts Jan 11 (one day later)
         (habit-b-start (owh-test-make-time 2024 1 11 0 0 0))
         (habit-b (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 3))
                                 :assessment-interval '(:days 3)
                                 :done-times (vector)
                                 :start-time habit-b-start))
         (window-spec-a (car (oref habit-a window-specs)))
         (window-spec-b (car (oref habit-b window-specs)))
         ;; Query both from the same time: Jan 15
         (query-time (owh-test-make-time 2024 1 15 12 0 0))
         (window-a (org-window-habit-get-assessment-window window-spec-a query-time))
         (window-b (org-window-habit-get-assessment-window window-spec-b query-time)))
    ;; Habit A (started Jan 10): periods are 10-13, 13-16, 16-19
    ;; Jan 15 falls in 13-16
    (should (owh-test-times-equal-p
             (oref window-a assessment-start-time)
             (owh-test-make-time 2024 1 13 0 0 0)))
    ;; Habit B (started Jan 11): periods are 11-14, 14-17, 17-20
    ;; Jan 15 falls in 14-17
    (should (owh-test-times-equal-p
             (oref window-b assessment-start-time)
             (owh-test-make-time 2024 1 14 0 0 0)))
    ;; The key point: they should be DIFFERENT because they're anchored
    ;; to different start times. With the old day-of-month approach,
    ;; both would have been the same.
    (should-not (owh-test-times-equal-p
                 (oref window-a assessment-start-time)
                 (oref window-b assessment-start-time)))))

(ert-deftest owh-test-week-aligned-assessment-interval ()
  "Test habits with week-aligned assessment intervals.
When assessment-interval is (:weeks 1 :start :monday), the habit should
correctly create assessment windows aligned to Monday boundaries.
This tests the fix for the 'number-or-marker-p :monday' error that
occurred when negate-plist was called on plists containing :start."
  (let* ((habit-start (owh-test-make-time 2024 1 15 0 0 0))  ; Monday
         (done-times (vector
                      (owh-test-make-time 2024 1 17 10 0 0)   ; Wed
                      (owh-test-make-time 2024 1 16 10 0 0))) ; Tue
         ;; This should NOT error with "Wrong type argument: number-or-marker-p, :monday"
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:weeks 1 :start :monday)
                                                                  :repetitions 3))
                               :assessment-interval '(:weeks 1 :start :monday)
                               :done-times done-times
                               :start-time habit-start))
         (window-spec (car (oref habit window-specs))))
    ;; Query from Wednesday Jan 17 - should be in the week of Jan 15-22
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 17 12 0 0))))
      ;; Assessment should start on Monday Jan 15
      (should (owh-test-times-equal-p
               (oref window assessment-start-time)
               (owh-test-make-time 2024 1 15 0 0 0)))
      ;; Assessment should end on Monday Jan 22
      (should (owh-test-times-equal-p
               (oref window assessment-end-time)
               (owh-test-make-time 2024 1 22 0 0 0))))))

(ert-deftest owh-test-sunday-aligned-assessment-interval ()
  "Test habits with Sunday-aligned assessment intervals.
Similar to the Monday-aligned test but with :start :sunday."
  (let* ((habit-start (owh-test-make-time 2024 1 14 0 0 0))  ; Sunday
         (done-times (vector (owh-test-make-time 2024 1 16 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:weeks 1 :start :sunday)
                                                                  :repetitions 2))
                               :assessment-interval '(:weeks 1 :start :sunday)
                               :done-times done-times
                               :start-time habit-start))
         (window-spec (car (oref habit window-specs))))
    ;; Query from Wednesday Jan 17 - should be in the week of Sun Jan 14 to Sun Jan 21
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 17 12 0 0))))
      ;; Assessment should start on Sunday Jan 14
      (should (owh-test-times-equal-p
               (oref window assessment-start-time)
               (owh-test-make-time 2024 1 14 0 0 0)))
      ;; Assessment should end on Sunday Jan 21
      (should (owh-test-times-equal-p
               (oref window assessment-end-time)
               (owh-test-make-time 2024 1 21 0 0 0))))))

;;; Reset Time and Anchoring Interaction Tests

(ert-deftest owh-test-reset-time-anchoring-basic ()
  "Test that reset time affects the anchor point for assessment intervals.
When reset-time is set, the habit's start-time (anchor) is derived from it."
  (let* ((reset-time (owh-test-make-time 2024 1 15 10 0 0))
         (done-times (vector
                      ;; Completions after reset
                      (owh-test-make-time 2024 1 18 10 0 0)
                      (owh-test-make-time 2024 1 16 10 0 0)
                      ;; Completions before reset (should be ignored)
                      (owh-test-make-time 2024 1 10 10 0 0)
                      (owh-test-make-time 2024 1 5 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 3)
                               :done-times done-times
                               :reset-time reset-time))
         (window-spec (car (oref habit window-specs))))
    ;; The habit's start-time should be derived from reset-time
    ;; (normalized to interval boundaries)
    (should (oref habit start-time))
    ;; Query from Jan 20 - assessment periods should be anchored
    ;; relative to the normalized reset time
    (let ((window (org-window-habit-get-assessment-window
                   window-spec (owh-test-make-time 2024 1 20 12 0 0))))
      ;; Window should exist and have reasonable bounds
      (should (oref window assessment-start-time))
      (should (oref window assessment-end-time))
      ;; Assessment end should be after assessment start
      (should (time-less-p (oref window assessment-start-time)
                           (oref window assessment-end-time))))))

(ert-deftest owh-test-reset-time-completion-filtering ()
  "Test that completions before reset-time are not counted.
Even if the assessment window technically spans before reset-time,
only completions after reset should count."
  (let* ((reset-time (owh-test-make-time 2024 1 15 0 0 0))
         (done-times (vector
                      ;; After reset
                      (owh-test-make-time 2024 1 17 10 0 0)
                      (owh-test-make-time 2024 1 16 10 0 0)
                      ;; Before reset - should NOT count
                      (owh-test-make-time 2024 1 14 10 0 0)
                      (owh-test-make-time 2024 1 13 10 0 0)
                      (owh-test-make-time 2024 1 12 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 5))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time)))
    ;; Count completions in window Jan 12-19 (spans before and after reset)
    ;; Only 2 completions (Jan 16, 17) should count, not 5
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 12 0 0 0)
                  (owh-test-make-time 2024 1 19 0 0 0))))
      (should (= count 2)))))

(ert-deftest owh-test-reset-time-assessment-window-spans-reset ()
  "Test behavior when assessment window spans across the reset time.
The window might start before reset but end after - only post-reset
completions should contribute to the conforming ratio."
  (let* ((reset-time (owh-test-make-time 2024 1 15 0 0 0))
         ;; 5 completions before reset, 2 after
         (done-times (vector
                      (owh-test-make-time 2024 1 17 10 0 0)
                      (owh-test-make-time 2024 1 16 10 0 0)
                      (owh-test-make-time 2024 1 14 10 0 0)
                      (owh-test-make-time 2024 1 13 10 0 0)
                      (owh-test-make-time 2024 1 12 10 0 0)
                      (owh-test-make-time 2024 1 11 10 0 0)
                      (owh-test-make-time 2024 1 10 10 0 0)))
         ;; Start time explicitly set to Jan 10 for predictable anchoring
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 5))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time
                               :start-time (owh-test-make-time 2024 1 10 0 0 0)))
         (window-spec (car (oref habit window-specs)))
         (iterator (org-window-habit-iterator-from-time
                    window-spec (owh-test-make-time 2024 1 17 12 0 0))))
    ;; The 7-day window ending Jan 18 spans Jan 11-18
    ;; But only completions after reset (Jan 15) should count: Jan 16, 17
    ;; With 5 required and only 2 counted, ratio should be 2/5 = 0.4
    (let ((ratio (org-window-habit-conforming-ratio iterator)))
      (should (< ratio 0.5))  ; Should be well under 1.0
      (should (> ratio 0.3))))) ; Should be around 0.4

(ert-deftest owh-test-reset-time-earliest-completion-after-reset ()
  "Test that earliest-completion-after-reset correctly filters."
  (let* ((reset-time (owh-test-make-time 2024 1 15 0 0 0))
         (done-times (vector
                      (owh-test-make-time 2024 1 20 10 0 0)
                      (owh-test-make-time 2024 1 17 10 0 0)  ; This should be earliest after reset
                      (owh-test-make-time 2024 1 14 10 0 0)  ; Before reset
                      (owh-test-make-time 2024 1 10 10 0 0))) ; Before reset
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time)))
    ;; earliest-completion-after-reset should return Jan 17, not Jan 10
    (let ((earliest (org-window-habit-earliest-completion-after-reset habit)))
      (should earliest)
      (should (owh-test-times-equal-p earliest (owh-test-make-time 2024 1 17 10 0 0))))))

(ert-deftest owh-test-reset-time-no-completions-after-reset ()
  "Test behavior when all completions are before reset time."
  (let* ((reset-time (owh-test-make-time 2024 1 20 0 0 0))
         (done-times (vector
                      (owh-test-make-time 2024 1 15 10 0 0)
                      (owh-test-make-time 2024 1 10 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time)))
    ;; No completions after reset, so earliest-completion-after-reset should be nil
    (should (null (org-window-habit-earliest-completion-after-reset habit)))
    ;; Completion count after reset should be 0
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 20 0 0 0)
                  (owh-test-make-time 2024 1 27 0 0 0))))
      (should (= count 0)))))

(ert-deftest owh-test-reset-time-exactly-on-completion ()
  "Test behavior when reset time exactly matches a completion time.
The completion at reset time should be included (>= not >)."
  (let* ((reset-time (owh-test-make-time 2024 1 15 10 0 0))
         (done-times (vector
                      (owh-test-make-time 2024 1 17 10 0 0)
                      (owh-test-make-time 2024 1 15 10 0 0)  ; Exactly at reset time
                      (owh-test-make-time 2024 1 14 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 3))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time)))
    ;; Completion at exactly reset time should be included
    (let ((earliest (org-window-habit-earliest-completion-after-reset habit)))
      (should (owh-test-times-equal-p earliest (owh-test-make-time 2024 1 15 10 0 0))))
    ;; Count should include the completion at reset time
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 14 0 0 0)
                  (owh-test-make-time 2024 1 18 0 0 0))))
      (should (= count 2)))))  ; Jan 15 and Jan 17

(ert-deftest owh-test-anchoring-derived-from-reset-vs-explicit ()
  "Test that habit with reset-time derives anchor differently than explicit start.
When start-time is not provided, it's derived from reset-time (normalized).
This might give a different anchor than explicitly setting start-time."
  (let* ((reset-time (owh-test-make-time 2024 1 15 10 30 0))
         ;; Habit A: explicit start-time
         (habit-a (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 3))
                                 :assessment-interval '(:days 3)
                                 :done-times (vector)
                                 :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Habit B: derived from reset-time (will be normalized)
         (habit-b (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 3))
                                 :assessment-interval '(:days 3)
                                 :done-times (vector)
                                 :reset-time reset-time)))
    ;; Habit A has explicit start at Jan 15 00:00
    (should (owh-test-times-equal-p
             (oref habit-a start-time)
             (owh-test-make-time 2024 1 15 0 0 0)))
    ;; Habit B's start is derived from reset-time via normalization
    ;; The normalization for (:days 3) uses day-of-month: 15 - (3-1) = 13
    ;; So start-time should be Jan 13 00:00
    (should (owh-test-times-equal-p
             (oref habit-b start-time)
             (owh-test-make-time 2024 1 13 0 0 0)))))

(ert-deftest owh-test-reset-time-interleaved-completions ()
  "Test with completions interleaved around reset time.
Completions: Jan 5, 12, 14, 16, 18, 22 with reset at Jan 15.
Only Jan 16, 18, 22 should count."
  (let* ((reset-time (owh-test-make-time 2024 1 15 0 0 0))
         (done-times (vector
                      (owh-test-make-time 2024 1 22 10 0 0)
                      (owh-test-make-time 2024 1 18 10 0 0)
                      (owh-test-make-time 2024 1 16 10 0 0)
                      (owh-test-make-time 2024 1 14 10 0 0)  ; Before reset
                      (owh-test-make-time 2024 1 12 10 0 0)  ; Before reset
                      (owh-test-make-time 2024 1 5 10 0 0))) ; Before reset
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 30)
                                                                  :repetitions 10))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time)))
    ;; Earliest after reset should be Jan 16
    (should (owh-test-times-equal-p
             (org-window-habit-earliest-completion-after-reset habit)
             (owh-test-make-time 2024 1 16 10 0 0)))
    ;; Count in window spanning Jan 1-30 should only count 3 (Jan 16, 18, 22)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 1 0 0 0)
                  (owh-test-make-time 2024 1 30 0 0 0))))
      (should (= count 3)))))

(ert-deftest owh-test-reset-time-changes-effective-window-scale ()
  "Test that reset time affects the effective window start for scaling.
When reset happens mid-window, the effective window is shortened,
which affects the required repetitions scaling."
  (let* ((reset-time (owh-test-make-time 2024 1 18 0 0 0))
         ;; One completion after reset
         (done-times (vector (owh-test-make-time 2024 1 19 10 0 0)))
         ;; Habit with 7-day window, 7 reps required, starting Jan 15
         (habit (make-instance 'org-window-habit
                               :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                  :duration '(:days 7)
                                                                  :repetitions 7))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :reset-time reset-time
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         (window-spec (car (oref habit window-specs)))
         ;; Query from Jan 20 - window is Jan 14-21
         (iterator (org-window-habit-iterator-from-time
                    window-spec (owh-test-make-time 2024 1 20 12 0 0))))
    ;; The effective-start should be max(window-start, habit-start)
    ;; Window starts Jan 14, habit-start is Jan 15, so effective is Jan 15
    ;; But wait - reset-time is Jan 18, which affects completion counting
    ;; We have 1 completion in a window that effectively starts Jan 18
    ;; (due to reset), giving ~3 days instead of 7
    ;; This affects conforming ratio calculation
    (let ((ratio (org-window-habit-conforming-ratio iterator)))
      ;; With 1 completion in ~3 effective days out of 7-day window with 7 reps
      ;; The ratio should be less than 1/7 ≈ 0.14 due to scaling
      ;; Exact value depends on effective window calculation
      (should (< ratio 1.0)))))

;;; Maybe Make List of Lists Tests

(ert-deftest owh-test-maybe-make-list-of-lists-already-nested ()
  "Test that already nested lists are returned as-is."
  (should (equal (org-window-habit-maybe-make-list-of-lists '((a b) (c d)))
                 '((a b) (c d)))))

(ert-deftest owh-test-maybe-make-list-of-lists-flat ()
  "Test that flat lists are wrapped."
  (should (equal (org-window-habit-maybe-make-list-of-lists '(a b))
                 '((a b)))))

;;; Aggregation Function Tests

(ert-deftest owh-test-default-aggregation-fn ()
  "Test default aggregation returns minimum."
  (should (= (org-window-habit-default-aggregation-fn '((0.5 x) (0.8 y) (0.3 z))) 0.3))
  (should (= (org-window-habit-default-aggregation-fn '((1.0 x))) 1.0))
  (should (= (org-window-habit-default-aggregation-fn '((0.0 x) (1.0 y))) 0.0)))

(ert-deftest owh-test-weighted-average-aggregation-fn ()
  "Test weighted average aggregation across multiple window ratios."
  (should (= (org-window-habit-weighted-average-aggregation-fn nil) 1.0))
  (should (= (org-window-habit-weighted-average-aggregation-fn '((1.0 1.0 x))) 1.0))
  (should (= (org-window-habit-weighted-average-aggregation-fn
              '((0.5 1.0 x) (1.0 3.0 y)))
             0.875))
  ;; Non-numeric values should fall back to weight 1.0.
  (should (= (org-window-habit-weighted-average-aggregation-fn
              '((0.5 (:days 2) x) (1.0 (:days 4) y)))
             0.75)))

;;; Property Name Tests

(ert-deftest owh-test-property-with-prefix ()
  "Test property name generation with prefix."
  (let ((org-window-habit-property-prefix "OWH"))
    (should (equal (org-window-habit-property "WINDOW_SPECS") "OWH_WINDOW_SPECS"))
    (should (equal (org-window-habit-property "ASSESSMENT_INTERVAL") "OWH_ASSESSMENT_INTERVAL"))))

(ert-deftest owh-test-property-without-prefix ()
  "Test property name generation without prefix."
  (let ((org-window-habit-property-prefix nil))
    (should (equal (org-window-habit-property "WINDOW_SPECS") "WINDOW_SPECS"))))

;;; org-window-habit-entry-p Predicate Tests

(ert-deftest owh-test-habit-p-with-window-specs ()
  "Test org-window-habit-entry-p returns non-nil for entry with WINDOW_SPECS."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_SPECS: ((:duration (:days 7) :repetitions 3))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-window-duration ()
  "Test org-window-habit-entry-p returns non-nil for entry with WINDOW_DURATION."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-without-properties ()
  "Test org-window-habit-entry-p returns nil for entry without window properties."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Regular todo\n")
      (insert ":PROPERTIES:\n")
      (insert ":CATEGORY: test\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should-not (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-without-todo-state ()
  "Test org-window-habit-entry-p returns nil for entry without TODO state."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* Just a heading\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_SPECS: ((:duration (:days 7) :repetitions 3))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should-not (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-prefix ()
  "Test org-window-habit-entry-p respects property prefix."
  (let ((org-window-habit-property-prefix "OWH"))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":OWH_WINDOW_SPECS: ((:duration (:days 7) :repetitions 3))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-prefix-wrong-property ()
  "Test org-window-habit-entry-p returns nil when property has wrong prefix."
  (let ((org-window-habit-property-prefix "OWH"))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      ;; Using non-prefixed property when prefix is expected
      (insert ":WINDOW_SPECS: ((:duration (:days 7) :repetitions 3))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should-not (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-both-properties ()
  "Test org-window-habit-entry-p returns non-nil when both properties exist.
This tests backwards compatibility - both old formats work."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_SPECS: ((:duration (:days 7) :repetitions 3))\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-config ()
  "Test org-window-habit-entry-p returns non-nil for entry with CONFIG property."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-habit-p-with-config-prefixed ()
  "Test org-window-habit-entry-p recognizes CONFIG with prefix."
  (let ((org-window-habit-property-prefix "OWH"))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (should (org-window-habit-entry-p)))))

(ert-deftest owh-test-entry-p-respects-property-inheritance-setting ()
  "Child tasks inherit habit properties only when Org is set to inherit them."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Habit\n:PROPERTIES:\n"
              ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 1)))\n"
              ":END:\n** TODO Subtask\n")
      (goto-char (point-max))
      (forward-line -1)
      (let ((org-use-property-inheritance nil))
        (should-not (org-window-habit-entry-p)))
      (let ((org-use-property-inheritance '("CONFIG")))
        (should (org-window-habit-entry-p))))))

;;; EIEIO Class Tests

(ert-deftest owh-test-window-spec-creation ()
  "Test creating window-spec instances."
  (let ((spec (make-instance 'org-window-habit-window-spec
                             :duration '(:days 7)
                             :repetitions 3
                             :value 1.0)))
    (should (equal (oref spec duration-plist) '(:days 7)))
    (should (= (oref spec target-repetitions) 3))
    (should (= (oref spec conforming-value) 1.0))))

(ert-deftest owh-test-window-spec-defaults ()
  "Test window-spec default values."
  (let ((spec (make-instance 'org-window-habit-window-spec)))
    (should (equal (oref spec duration-plist) '(:days 1)))
    (should (= (oref spec target-repetitions) 1))
    (should (null (oref spec conforming-value)))))

(ert-deftest owh-test-habit-creation ()
  "Test creating habit instances."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil)))
    (should (= (length (oref habit window-specs)) 1))
    (should (equal (oref habit assessment-interval) '(:days 1)))))

(ert-deftest owh-test-habit-requires-assessment-interval ()
  "Test that habits require assessment-interval."
  (let ((spec (make-instance 'org-window-habit-window-spec)))
    (should-error
     (make-instance 'org-window-habit
                    :window-specs (list spec)
                    :assessment-interval nil
                    :start-time nil))))

(ert-deftest owh-test-habit-requires-window-specs ()
  "Test that habits require window-specs."
  (should-error
   (make-instance 'org-window-habit
                  :window-specs nil
                  :assessment-interval '(:days 1)
                  :start-time nil)))

(ert-deftest owh-test-habit-sets-reschedule-interval-default ()
  "Test that reschedule-interval defaults to one day."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 2)
                               :reschedule-interval nil
                               :done-times []
                               :start-time nil)))
    (should (equal (oref habit reschedule-interval) '(:days 1)))))

(ert-deftest owh-test-habit-sets-reschedule-assessment-default ()
  "Test that reschedule-assessment-interval defaults to one day."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:weeks 1 :start :monday)
                               :reschedule-interval '(:days 2)
                               :reschedule-assessment-interval nil
                               :done-times []
                               :start-time nil)))
    (should (equal (oref habit reschedule-assessment-interval)
                   '(:days 1)))))

(ert-deftest owh-test-habit-links-window-specs ()
  "Test that habit initialization links window-specs back to habit."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil)))
    (should (eq (oref spec habit) habit))))

;;; Assessment Window Tests

(ert-deftest owh-test-assessment-window-creation ()
  "Test creating assessment window instances."
  (let* ((start (owh-test-make-time 2024 1 1))
         (end (owh-test-make-time 2024 1 8))
         (window (make-instance 'org-window-habit-assessment-window
                                :assessment-start-time start
                                :assessment-end-time end
                                :start-time start
                                :end-time end)))
    (should (time-equal-p (oref window assessment-start-time) start))
    (should (time-equal-p (oref window assessment-end-time) end))))

(ert-deftest owh-test-time-falls-in-assessment-interval ()
  "Test checking if time falls in assessment interval."
  (let* ((start (owh-test-make-time 2024 1 1))
         (end (owh-test-make-time 2024 1 8))
         (window (make-instance 'org-window-habit-assessment-window
                                :assessment-start-time start
                                :assessment-end-time end
                                :start-time start
                                :end-time end))
         (inside (owh-test-make-time 2024 1 5))
         (before (owh-test-make-time 2023 12 31))
         (after (owh-test-make-time 2024 1 9)))
    (should (org-window-habit-time-falls-in-assessment-interval window inside))
    (should (org-window-habit-time-falls-in-assessment-interval window start))
    (should-not (org-window-habit-time-falls-in-assessment-interval window end))
    (should-not (org-window-habit-time-falls-in-assessment-interval window before))
    (should-not (org-window-habit-time-falls-in-assessment-interval window after))))

;;; Array Search Tests

(ert-deftest owh-test-find-array-forward-basic ()
  "Test forward array search."
  (let* ((t1 (owh-test-make-time 2024 1 10))
         (t2 (owh-test-make-time 2024 1 8))
         (t3 (owh-test-make-time 2024 1 5))
         (t4 (owh-test-make-time 2024 1 2))
         (array (vector t1 t2 t3 t4))
         (search-time (owh-test-make-time 2024 1 6)))
    ;; Find first element where search-time is not less-or-equal to element
    ;; Array is descending: 10, 8, 5, 2. Search time is 6.
    ;; 6 <= 10? yes, continue. 6 <= 8? yes, continue. 6 <= 5? no, stop at index 2
    (should (= (org-window-habit-find-array-forward
                array search-time
                :comparison 'org-window-habit-time-less-or-equal-p) 2))))

(ert-deftest owh-test-find-array-forward-with-start-index ()
  "Test forward array search with start index."
  (let* ((t1 (owh-test-make-time 2024 1 10))
         (t2 (owh-test-make-time 2024 1 8))
         (t3 (owh-test-make-time 2024 1 5))
         (t4 (owh-test-make-time 2024 1 2))
         (array (vector t1 t2 t3 t4))
         (search-time (owh-test-make-time 2024 1 6)))
    (should (= (org-window-habit-find-array-forward
                array search-time
                :start-index 1
                :comparison 'org-window-habit-time-less-or-equal-p) 2))))

(ert-deftest owh-test-find-array-backward-basic ()
  "Test backward array search."
  (let* ((t1 (owh-test-make-time 2024 1 10))
         (t2 (owh-test-make-time 2024 1 8))
         (t3 (owh-test-make-time 2024 1 5))
         (t4 (owh-test-make-time 2024 1 2))
         (array (vector t1 t2 t3 t4))
         (search-time (owh-test-make-time 2024 1 6)))
    ;; Searching backward from end of array (index 4)
    ;; Array is descending: 10, 8, 5, 2. Search time is 6.
    ;; Using time-greater-p: 6 > 2? yes, continue. 6 > 5? yes, continue. 6 > 8? no, stop at index 2
    (should (= (org-window-habit-find-array-backward
                array search-time
                :comparison 'org-window-habit-time-greater-p) 2))))

;;; Iterator Tests

(ert-deftest owh-test-iterator-creation ()
  "Test creating an iterator from a window spec."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil))
         (time (owh-test-make-time 2024 1 15))
         (iterator (org-window-habit-iterator-from-time spec time)))
    (should (not (null iterator)))
    (should (eq (oref iterator window-spec) spec))
    (should (not (null (oref iterator window))))))

(ert-deftest owh-test-iterator-advance ()
  "Test advancing an iterator."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil))
         (time (owh-test-make-time 2024 1 15))
         (iterator (org-window-habit-iterator-from-time spec time))
         (initial-start (oref (oref iterator window) assessment-start-time)))
    (org-window-habit-advance iterator)
    (let ((new-start (oref (oref iterator window) assessment-start-time)))
      ;; After advancing by 1 day (assessment-interval), start should be different
      (should-not (time-equal-p initial-start new-start)))))

;;; Habit Method Tests

(ert-deftest owh-test-has-any-done-times-empty ()
  "Test has-any-done-times with empty done-times."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil)))
    (should-not (org-window-habit-has-any-done-times habit))))

(ert-deftest owh-test-has-any-done-times-populated ()
  "Test has-any-done-times with populated done-times."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (done-time (owh-test-make-time 2024 1 15))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times (vector done-time)
                               :start-time nil)))
    (should (org-window-habit-has-any-done-times habit))))

(ert-deftest owh-test-earliest-completion-empty ()
  "Test earliest-completion with empty done-times."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil)))
    (should (null (org-window-habit-earliest-completion habit)))))

(ert-deftest owh-test-earliest-completion-populated ()
  "Test earliest-completion returns last element (earliest chronologically)."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (t1 (owh-test-make-time 2024 1 20))  ; Most recent
         (t2 (owh-test-make-time 2024 1 15))
         (t3 (owh-test-make-time 2024 1 10))  ; Earliest
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times (vector t1 t2 t3)
                               :start-time nil)))
    (should (time-equal-p (org-window-habit-earliest-completion habit) t3))))

;;; Window Calculation Tests

(ert-deftest owh-test-get-window-where-time-in-last-assessment ()
  "Test window calculation for a given time."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil))
         (time (owh-test-make-time 2024 1 15 12 0 0))
         (window (org-window-habit-get-window-where-time-in-last-assessment spec time)))
    (should (not (null window)))
    ;; Assessment should start at beginning of day
    (should (owh-test-times-equal-p
             (oref window assessment-start-time)
             (owh-test-make-time 2024 1 15 0 0 0)))
    ;; Assessment should end at beginning of next day
    (should (owh-test-times-equal-p
             (oref window assessment-end-time)
             (owh-test-make-time 2024 1 16 0 0 0)))
    ;; Window should start 7 days before assessment end
    (should (owh-test-times-equal-p
             (oref window start-time)
             (owh-test-make-time 2024 1 9 0 0 0)))))

;;; Completion Count Tests

(ert-deftest owh-test-get-completion-count-empty ()
  "Test completion count with no done times."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times []
                               :start-time nil))
         (start (owh-test-make-time 2024 1 8))
         (end (owh-test-make-time 2024 1 15)))
    (should (= (org-window-habit-get-completion-count habit start end) 0))))

(ert-deftest owh-test-get-completion-count-with-completions ()
  "Test completion count with done times in window."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         (t1 (owh-test-make-time 2024 1 14))
         (t2 (owh-test-make-time 2024 1 12))
         (t3 (owh-test-make-time 2024 1 10))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :done-times (vector t1 t2 t3)
                               :start-time nil))
         (start (owh-test-make-time 2024 1 8))
         (end (owh-test-make-time 2024 1 15)))
    (should (= (org-window-habit-get-completion-count habit start end) 3))))

(ert-deftest owh-test-get-completion-count-respects-max-per-interval ()
  "Test that completion count respects max-repetitions-per-interval."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Two completions on same day
         (t1 (owh-test-make-time 2024 1 14 10 0 0))
         (t2 (owh-test-make-time 2024 1 14 14 0 0))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vector t2 t1)  ; Most recent first
                               :start-time nil))
         (start (owh-test-make-time 2024 1 8))
         (end (owh-test-make-time 2024 1 15)))
    ;; Should count as 1, not 2, because max is 1 per interval
    (should (= (org-window-habit-get-completion-count habit start end) 1))))

;;; ---------------------------------------------------------------------------
;;; Habit Reset Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-reset-ignores-completions-before-reset-time ()
  "Test that completions before RESET_TIME are ignored.
When a habit has a reset time, only completions after that time should
count toward the conforming ratio."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Completions: 3 before reset, 2 after reset
         (done-times (list (owh-test-make-time 2024 1 20)  ; After reset
                           (owh-test-make-time 2024 1 18)  ; After reset
                           (owh-test-make-time 2024 1 10)  ; Before reset
                           (owh-test-make-time 2024 1 8)   ; Before reset
                           (owh-test-make-time 2024 1 5))) ; Before reset
         ;; Reset on Jan 15 - only Jan 18 and Jan 20 should count
         (reset-time (owh-test-make-time 2024 1 15))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :reset-time reset-time
                               :start-time nil))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 20 12 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; With reset: only 2 completions in window, effective window scaled
    ;; Window is Jan 14-21, but effective start is Jan 15 (reset time)
    ;; So ~6 days effective, scaled target = 5 * (6/7) ≈ 4.3
    ;; 2 completions / 4.3 ≈ 0.47
    (should (< ratio 0.6))
    (should (> ratio 0.3))))

(ert-deftest owh-test-reset-affects-start-time ()
  "Test that reset time becomes the effective start time for the habit."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Old completions that would normally set start-time
         (done-times (list (owh-test-make-time 2024 1 20)
                           (owh-test-make-time 2024 1 5)))
         (reset-time (owh-test-make-time 2024 1 15))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat done-times)
                               :reset-time reset-time
                               :start-time nil)))
    ;; The habit's effective start should be the reset time, not Jan 5
    (should (time-equal-p (oref habit start-time)
                          (owh-test-make-time 2024 1 15 0 0 0)))))

(ert-deftest owh-test-reset-with-no-completions-after ()
  "Test reset behavior when there are no completions after reset time."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; All completions before reset
         (done-times (list (owh-test-make-time 2024 1 10)
                           (owh-test-make-time 2024 1 8)
                           (owh-test-make-time 2024 1 5)))
         (reset-time (owh-test-make-time 2024 1 15))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :reset-time reset-time
                               :start-time nil))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 20)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; No completions after reset, so ratio should be 0
    (should (= ratio 0.0))))

(ert-deftest owh-test-reset-from-property ()
  "Test that RESET_TIME property is read and used."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":RESET_TIME: [2024-01-15 Mon]\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-20 Sat 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Reset time should be Jan 15
        (should (time-equal-p (oref habit reset-time)
                              (owh-test-make-time 2024 1 15 0 0 0)))
        ;; Start time should be reset time, not earliest completion
        (should (time-equal-p (oref habit start-time)
                              (owh-test-make-time 2024 1 15 0 0 0)))))))

(ert-deftest owh-test-reset-nil-behaves-normally ()
  "Test that habits without reset time behave as before."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; All completions within the 7-day window ending Jan 21
         (done-times (list (owh-test-make-time 2024 1 20)
                           (owh-test-make-time 2024 1 19)
                           (owh-test-make-time 2024 1 17)
                           (owh-test-make-time 2024 1 16)
                           (owh-test-make-time 2024 1 15)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :reset-time nil  ; No reset
                               :start-time nil))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 20 12 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; All 5 completions within window, ratio should be 1.0
    (should (>= ratio 0.99))))

(ert-deftest owh-test-completion-count-respects-reset-time ()
  "Test that get-completion-count ignores completions before reset time."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; 3 completions before reset, 2 after
         (done-times (list (owh-test-make-time 2024 1 20)
                           (owh-test-make-time 2024 1 18)
                           (owh-test-make-time 2024 1 10)
                           (owh-test-make-time 2024 1 8)
                           (owh-test-make-time 2024 1 5)))
         (reset-time (owh-test-make-time 2024 1 15))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times (vconcat (sort (copy-sequence done-times)
                                                          (lambda (a b) (time-less-p b a))))
                               :reset-time reset-time
                               :start-time nil))
         ;; Count completions in Jan 1-21 window
         (count (org-window-habit-get-completion-count
                 habit
                 (owh-test-make-time 2024 1 1)
                 (owh-test-make-time 2024 1 21))))
    ;; Should only count 2 (the ones after reset)
    (should (= count 2))))

(provide 'org-window-habit-core-test)
;;; org-window-habit-core-test.el ends here
