;;; org-window-habit-days-test.el --- Day-of-week restriction tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Day-of-week restriction tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; ---------------------------------------------------------------------------
;;; ONLY_DAYS Feature Tests (Day-of-Week Filtering)
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-only-days-completion-filtering ()
  "Test that ONLY_DAYS filters which completions count.
With only-days set to M/W/F, completions on other days shouldn't count."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Week of Jan 15-21, 2024:
         ;; Mon 15, Tue 16, Wed 17, Thu 18, Fri 19, Sat 20, Sun 21
         ;; Completions on Mon, Tue, Wed, Thu, Fri (5 total)
         ;; But only M/W/F should count (3)
         (done-times (vector
                      (owh-test-make-time 2024 1 19 10 0 0)  ; Friday
                      (owh-test-make-time 2024 1 18 10 0 0)  ; Thursday - shouldn't count
                      (owh-test-make-time 2024 1 17 10 0 0)  ; Wednesday
                      (owh-test-make-time 2024 1 16 10 0 0)  ; Tuesday - shouldn't count
                      (owh-test-make-time 2024 1 15 10 0 0))) ; Monday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; Only 3 should count (Mon, Wed, Fri)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
      (should (= count 3)))))

(ert-deftest owh-test-only-days-mwf-conformity ()
  "Test M/W/F habit conformity when all required days are completed."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completions on Mon, Wed, Fri only
         (done-times (vector
                      (owh-test-make-time 2024 1 19 10 0 0)  ; Friday
                      (owh-test-make-time 2024 1 17 10 0 0)  ; Wednesday
                      (owh-test-make-time 2024 1 15 10 0 0))) ; Monday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               ;; Start early enough so full 7-day window is active
                               :start-time (owh-test-make-time 2024 1 8 0 0 0)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 19 23 0 0))))
    ;; 3/3 = 100% conforming
    (should (>= (org-window-habit-conforming-ratio iterator) 0.99))))

(ert-deftest owh-test-only-days-missed-one ()
  "Test M/W/F habit when one required day is missed."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Only completed on Mon and Fri, missed Wed
         (done-times (vector
                      (owh-test-make-time 2024 1 19 10 0 0)  ; Friday
                      (owh-test-make-time 2024 1 15 10 0 0))) ; Monday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               ;; Start early enough so full 7-day window is active
                               :start-time (owh-test-make-time 2024 1 8 0 0 0)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 19 23 0 0))))
    ;; 2/3 ≈ 0.67
    (let ((ratio (org-window-habit-conforming-ratio iterator)))
      (should (< ratio 0.8))
      (should (> ratio 0.6)))))

(ert-deftest owh-test-only-days-completion-on-wrong-day-ignored ()
  "Test that completing on a non-allowed day doesn't count."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Tue, Thu, Sat - none of these are allowed!
         (done-times (vector
                      (owh-test-make-time 2024 1 20 10 0 0)  ; Saturday
                      (owh-test-make-time 2024 1 18 10 0 0)  ; Thursday
                      (owh-test-make-time 2024 1 16 10 0 0))) ; Tuesday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; 0 completions should count
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
      (should (= count 0)))))

(ert-deftest owh-test-only-days-weekend-habit ()
  "Test a weekend-only habit (Saturday and Sunday)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 2))
         ;; Completed on Sat and Sun
         (done-times (vector
                      (owh-test-make-time 2024 1 21 10 0 0)  ; Sunday
                      (owh-test-make-time 2024 1 20 10 0 0))) ; Saturday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:saturday :sunday)
                               ;; Start early enough so full 7-day window is active
                               :start-time (owh-test-make-time 2024 1 8 0 0 0)))
         (iterator (org-window-habit-iterator-from-time
                    spec (owh-test-make-time 2024 1 21 23 0 0))))
    ;; 2/2 = 100% conforming
    (should (>= (org-window-habit-conforming-ratio iterator) 0.99))))

(ert-deftest owh-test-only-days-rescheduling ()
  "Test that rescheduling targets only allowed days."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Monday Jan 15
         (done-times (vector (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Tuesday Jan 16
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 16 10 0 0))))
    ;; Should schedule to Wednesday Jan 17, not Tuesday
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 17 0 0 0)))))

(ert-deftest owh-test-only-days-nil-means-all-days ()
  "Test that nil/unset only-days means all days are allowed (default behavior)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Completions on various days
         (done-times (vector
                      (owh-test-make-time 2024 1 19 10 0 0)  ; Friday
                      (owh-test-make-time 2024 1 18 10 0 0)  ; Thursday
                      (owh-test-make-time 2024 1 17 10 0 0)  ; Wednesday
                      (owh-test-make-time 2024 1 16 10 0 0)  ; Tuesday
                      (owh-test-make-time 2024 1 15 10 0 0))) ; Monday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               ;; No only-days set (nil)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; All 5 should count
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
      (should (= count 5)))))

;;; Day-of-week convenience constants tests

(ert-deftest owh-test-weekdays-constant ()
  "Test that org-window-habit-weekdays contains the correct days."
  (should (equal org-window-habit-weekdays
                 '(:monday :tuesday :wednesday :thursday :friday))))

(ert-deftest owh-test-weekends-constant ()
  "Test that org-window-habit-weekends contains the correct days."
  (should (equal org-window-habit-weekends
                 '(:saturday :sunday))))

(ert-deftest owh-test-reschedule-days-with-weekdays-constant ()
  "Test using org-window-habit-weekdays with reschedule-days."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Saturday Jan 20 - should still count!
         (done-times (vector (owh-test-make-time 2024 1 20 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               ;; Using the convenience constant
                               :reschedule-days org-window-habit-weekdays
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; Saturday completion should count
    (should (= 1 (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))))

(ert-deftest owh-test-only-days-with-weekends-constant ()
  "Test using org-window-habit-weekends with only-days for a weekend-only habit."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 2))
         ;; Completed on Sat and Sun
         (done-times (vector
                      (owh-test-make-time 2024 1 21 10 0 0)  ; Sunday
                      (owh-test-make-time 2024 1 20 10 0 0))) ; Saturday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               ;; Using the convenience constant
                               :only-days org-window-habit-weekends
                               :start-time (owh-test-make-time 2024 1 8 0 0 0))))
    ;; Both weekend completions should count
    (should (= 2 (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))))

;;; RESCHEDULE-DAYS Tests
;;;
;;; These tests verify the reschedule-days property, which restricts which days
;;; a habit can be rescheduled to WITHOUT affecting which completions count.
;;;
;;; Key distinction from only-days:
;;; - only-days: Restricts both completion counting AND rescheduling
;;; - reschedule-days: Restricts ONLY rescheduling (all completions still count)
;;;
;;; Interaction: If only-days is set, reschedule-days must be a subset of only-days
;;; (it doesn't make sense to reschedule to a day where completions won't count).

(ert-deftest owh-test-reschedule-days-basic ()
  "Test that reschedule-days restricts rescheduling without affecting completion counting.
A habit with reschedule-days set to weekdays should:
- Count completions on ANY day (including weekends)
- Only reschedule to weekdays"
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Saturday Jan 20 - should still count!
         (done-times (vector (owh-test-make-time 2024 1 20 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :reschedule-days '(:monday :tuesday :wednesday :thursday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; Completion on Saturday should count (unlike only-days which would filter it)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
      (should (= count 1)))))

(ert-deftest owh-test-reschedule-days-rescheduling-skips-weekend ()
  "Test that reschedule-days causes rescheduling to skip non-allowed days.
If completed on Friday and reschedule-days is weekdays only,
next reschedule should be Monday, not Saturday."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Completed on Friday Jan 19
         (done-times (vector (owh-test-make-time 2024 1 19 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :reschedule-days '(:monday :tuesday :wednesday :thursday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Saturday Jan 20
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 20 10 0 0))))
    ;; Should schedule to Monday Jan 22, not Saturday Jan 20
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 22 0 0 0)))))

(ert-deftest owh-test-reschedule-days-weekend-completions-count ()
  "Test that weekend completions count when only reschedule-days restricts weekends.
With reschedule-days = weekdays, completions on Sat/Sun should still count toward goals."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Sat and Sun - both should count
         (done-times (vector
                      (owh-test-make-time 2024 1 21 10 0 0)  ; Sunday
                      (owh-test-make-time 2024 1 20 10 0 0)  ; Saturday
                      (owh-test-make-time 2024 1 19 10 0 0))) ; Friday
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :reschedule-days '(:monday :tuesday :wednesday :thursday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0))))
    ;; All 3 completions should count (Fri, Sat, Sun)
    (let ((count (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
      (should (= count 3)))))

(ert-deftest owh-test-reschedule-days-nil-means-all-days ()
  "Test that nil reschedule-days means rescheduling can happen on any day."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 5))
         ;; Completed on Friday Jan 19
         (done-times (vector (owh-test-make-time 2024 1 19 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               ;; No reschedule-days set (nil)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Saturday Jan 20
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 20 10 0 0))))
    ;; Should schedule to Saturday Jan 20 (any day allowed)
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 20 0 0 0)))))

(ert-deftest owh-test-reschedule-days-specific-days ()
  "Test reschedule-days with specific non-contiguous days (e.g., Mon/Wed/Fri)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Monday Jan 15
         (done-times (vector (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :reschedule-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Tuesday Jan 16
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 16 10 0 0))))
    ;; Should schedule to Wednesday Jan 17, skipping Tuesday
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 17 0 0 0)))))

;;; Interaction between only-days and reschedule-days

(ert-deftest owh-test-only-days-and-reschedule-days-subset ()
  "Test that reschedule-days as a subset of only-days works correctly.
With only-days = all weekdays and reschedule-days = M/W/F:
- Completions count only on weekdays (only-days)
- Rescheduling only to M/W/F (reschedule-days), skipping Tu/Th"
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 7))  ; Daily habit - needs completion every day
         ;; Completed only on Monday
         (done-times (vector (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :tuesday :wednesday :thursday :friday)
                               :reschedule-days '(:monday :wednesday :friday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Monday evening - habit immediately needs completion
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 15 20 0 0))))
    ;; Monday completion should count
    (should (= 1 (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
    ;; Next reschedule should be Wednesday (skips Tuesday which is in only-days
    ;; but NOT in reschedule-days)
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 17 0 0 0)))))

(ert-deftest owh-test-only-days-without-reschedule-days ()
  "Test that only-days alone still controls both counting and rescheduling.
Backward compatibility: if reschedule-days is nil, only-days controls rescheduling."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 3))
         ;; Completed on Monday
         (done-times (vector (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               :only-days '(:monday :wednesday :friday)
                               ;; No reschedule-days - only-days should control rescheduling
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Tuesday Jan 16
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 16 10 0 0))))
    ;; Should reschedule to Wednesday (next only-day), not Tuesday
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 17 0 0 0)))))

(ert-deftest owh-test-reschedule-days-not-subset-of-only-days-warning ()
  "Test behavior when reschedule-days is not a subset of only-days.
This is a potentially invalid configuration. The system should either:
- Warn the user, or
- Automatically intersect the two sets
This test documents the expected behavior (intersection)."
  (let* ((spec (make-instance 'org-window-habit-window-spec
                              :duration '(:days 7)
                              :repetitions 2))
         ;; Completed on Saturday (not in only-days, shouldn't count)
         (done-times (vector (owh-test-make-time 2024 1 20 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 1)
                               :max-repetitions-per-interval 1
                               :done-times done-times
                               ;; only-days restricts completions to M/W/F
                               :only-days '(:monday :wednesday :friday)
                               ;; reschedule-days includes Saturday which is NOT in only-days
                               ;; This is an invalid config - Sat reschedule wouldn't count anyway
                               :reschedule-days '(:wednesday :saturday)
                               :start-time (owh-test-make-time 2024 1 15 0 0 0)))
         ;; Query from Sunday Jan 21
         (next-required (org-window-habit-get-next-required-interval
                         habit (owh-test-make-time 2024 1 21 10 0 0))))
    ;; Saturday completion should NOT count (only-days filters it)
    (should (= 0 (org-window-habit-get-completion-count
                  habit
                  (owh-test-make-time 2024 1 15 0 0 0)
                  (owh-test-make-time 2024 1 22 0 0 0))))
    ;; Should reschedule to Wednesday (intersection of reschedule-days and only-days)
    ;; NOT Saturday (which is in reschedule-days but not only-days)
    (should (owh-test-times-equal-p
             next-required
             (owh-test-make-time 2024 1 24 0 0 0)))))  ; Wed Jan 24

(provide 'org-window-habit-days-test)
;;; org-window-habit-days-test.el ends here
