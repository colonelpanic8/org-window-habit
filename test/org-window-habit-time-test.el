;;; org-window-habit-time-test.el --- Time and duration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Time and duration tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; Duration/Plist Math Tests

(ert-deftest owh-test-negate-plist ()
  "Test negating duration plists."
  (should (equal (org-window-habit-negate-plist '(:days 1))
                 '(:days -1)))
  (should (equal (org-window-habit-negate-plist '(:days 7))
                 '(:days -7)))
  (should (equal (org-window-habit-negate-plist '(:months 2))
                 '(:months -2)))
  (should (equal (org-window-habit-negate-plist '(:hours 12))
                 '(:hours -12)))
  (should (equal (org-window-habit-negate-plist '(:days 1 :hours 6))
                 '(:days -1 :hours -6))))

(ert-deftest owh-test-multiply-plist ()
  "Test multiplying duration plists."
  (should (equal (org-window-habit-multiply-plist '(:days 1) 3)
                 '(:days 3)))
  (should (equal (org-window-habit-multiply-plist '(:days 2) 5)
                 '(:days 10)))
  (should (equal (org-window-habit-multiply-plist '(:hours 6) 4)
                 '(:hours 24)))
  (should (equal (org-window-habit-multiply-plist '(:days 1 :hours 2) 3)
                 '(:days 3 :hours 6))))

(ert-deftest owh-test-negate-plist-with-start ()
  "Test negating duration plists that include :start alignment.
Plists like (:weeks 1 :start :monday) should preserve the :start key
while negating the numeric values."
  (should (equal (org-window-habit-negate-plist '(:weeks 1 :start :monday))
                 '(:weeks -1 :start :monday)))
  (should (equal (org-window-habit-negate-plist '(:weeks 2 :start :sunday))
                 '(:weeks -2 :start :sunday)))
  (should (equal (org-window-habit-negate-plist '(:weeks 1 :start :friday))
                 '(:weeks -1 :start :friday))))

(ert-deftest owh-test-multiply-plist-with-start ()
  "Test multiplying duration plists that include :start alignment.
The :start value (a symbol like :monday) should be preserved unchanged."
  (should (equal (org-window-habit-multiply-plist '(:weeks 1 :start :monday) 2)
                 '(:weeks 2 :start :monday)))
  (should (equal (org-window-habit-multiply-plist '(:weeks 1 :start :sunday) 3)
                 '(:weeks 3 :start :sunday))))

(ert-deftest owh-test-duration-proportion ()
  "Test calculating duration proportions."
  (let* ((start (owh-test-make-time 2024 1 1 0 0 0))
         (end (owh-test-make-time 2024 1 2 0 0 0))
         (mid (owh-test-make-time 2024 1 1 12 0 0)))
    ;; At midpoint, proportion should be 0.5
    (should (< (abs (- (org-window-habit-duration-proportion start end mid) 0.5)) 0.01))
    ;; At start, proportion should be 1.0
    (should (< (abs (- (org-window-habit-duration-proportion start end start) 1.0)) 0.01))
    ;; At end, proportion should be 0.0
    (should (< (abs (- (org-window-habit-duration-proportion start end end) 0.0)) 0.01))))

(ert-deftest owh-test-duration-proportion-quarter ()
  "Test duration proportion at quarter points."
  (let* ((start (owh-test-make-time 2024 1 1 0 0 0))
         (end (owh-test-make-time 2024 1 5 0 0 0))   ; 4 days
         (quarter (owh-test-make-time 2024 1 4 0 0 0)))  ; 3 days in
    ;; 3 days in out of 4 means 1 day remaining, proportion = 0.25
    (should (< (abs (- (org-window-habit-duration-proportion start end quarter) 0.25)) 0.01))))

;;; Time Utility Tests

(ert-deftest owh-test-time-less-or-equal-p ()
  "Test time-less-or-equal-p function."
  (let ((t1 (owh-test-make-time 2024 1 1))
        (t2 (owh-test-make-time 2024 1 2))
        (t3 (owh-test-make-time 2024 1 1)))
    (should (org-window-habit-time-less-or-equal-p t1 t2))
    (should (org-window-habit-time-less-or-equal-p t1 t3))
    (should-not (org-window-habit-time-less-or-equal-p t2 t1))))

(ert-deftest owh-test-time-greater-p ()
  "Test time-greater-p function."
  (let ((t1 (owh-test-make-time 2024 1 1))
        (t2 (owh-test-make-time 2024 1 2)))
    (should (org-window-habit-time-greater-p t2 t1))
    (should-not (org-window-habit-time-greater-p t1 t2))
    (should-not (org-window-habit-time-greater-p t1 t1))))

(ert-deftest owh-test-time-greater-or-equal-p ()
  "Test time-greater-or-equal-p function."
  (let ((t1 (owh-test-make-time 2024 1 1))
        (t2 (owh-test-make-time 2024 1 2))
        (t3 (owh-test-make-time 2024 1 1)))
    (should (org-window-habit-time-greater-or-equal-p t2 t1))
    (should (org-window-habit-time-greater-or-equal-p t1 t3))
    (should-not (org-window-habit-time-greater-or-equal-p t1 t2))))

(ert-deftest owh-test-time-max ()
  "Test time-max function."
  (let ((t1 (owh-test-make-time 2024 1 1))
        (t2 (owh-test-make-time 2024 1 15))
        (t3 (owh-test-make-time 2024 1 10)))
    (should (time-equal-p (org-window-habit-time-max t1 t2 t3) t2))
    (should (time-equal-p (org-window-habit-time-max t1) t1))
    (should (time-equal-p (org-window-habit-time-max t3 t1) t3))))

;;; Keyed Duration Add Tests

(ert-deftest owh-test-keyed-duration-add-days ()
  "Test adding days to a time."
  (let* ((base (owh-test-make-time 2024 1 15 12 30 45))
         (result (org-window-habit-keyed-duration-add :base-time base :days 5)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 20 12 30 45)))))

(ert-deftest owh-test-keyed-duration-add-months ()
  "Test adding months to a time."
  (let* ((base (owh-test-make-time 2024 1 15))
         (result (org-window-habit-keyed-duration-add :base-time base :months 2)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 3 15)))))

(ert-deftest owh-test-keyed-duration-add-months-overflow ()
  "Test adding months that overflow to next year."
  (let* ((base (owh-test-make-time 2024 11 15))
         (result (org-window-habit-keyed-duration-add :base-time base :months 3)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2025 2 15)))))

(ert-deftest owh-test-keyed-duration-add-years ()
  "Test adding years to a time."
  (let* ((base (owh-test-make-time 2024 6 15))
         (result (org-window-habit-keyed-duration-add :base-time base :years 2)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2026 6 15)))))

(ert-deftest owh-test-keyed-duration-add-negative-days ()
  "Test subtracting days from a time."
  (let* ((base (owh-test-make-time 2024 1 15))
         (result (org-window-habit-keyed-duration-add :base-time base :days -5)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 10)))))

(ert-deftest owh-test-keyed-duration-add-plist ()
  "Test adding duration via plist."
  (let* ((base (owh-test-make-time 2024 1 15))
         (result (org-window-habit-keyed-duration-add-plist base '(:days 7))))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 22)))))

(ert-deftest owh-test-keyed-duration-add-hours ()
  "Test adding hours to a time."
  (let* ((base (owh-test-make-time 2024 1 15 10 0 0))
         (result (org-window-habit-keyed-duration-add :base-time base :hours 5)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 15 15 0 0)))))

;;; String Duration Parsing Tests

(ert-deftest owh-test-string-duration-to-plist-days ()
  "Test parsing day durations."
  (should (equal (org-window-habit-string-duration-to-plist "1d") '(:days 1)))
  (should (equal (org-window-habit-string-duration-to-plist "7d") '(:days 7)))
  (should (equal (org-window-habit-string-duration-to-plist "30D") '(:days 30))))

(ert-deftest owh-test-string-duration-to-plist-weeks ()
  "Test parsing week durations.
Note: '1w' shorthand converts to (:days 7), NOT (:weeks 1).
This means '1w' does not get week-day alignment.
Use (:weeks 1) or (:weeks 1 :start :monday) for true week alignment."
  (should (equal (org-window-habit-string-duration-to-plist "1w") '(:days 7)))
  (should (equal (org-window-habit-string-duration-to-plist "2W") '(:days 14)))
  (should (equal (org-window-habit-string-duration-to-plist "4w") '(:days 28))))

(ert-deftest owh-test-string-duration-to-plist-months ()
  "Test parsing month durations."
  (should (equal (org-window-habit-string-duration-to-plist "1m") '(:months 1)))
  (should (equal (org-window-habit-string-duration-to-plist "6M") '(:months 6))))

(ert-deftest owh-test-string-duration-to-plist-years ()
  "Test parsing year durations."
  (should (equal (org-window-habit-string-duration-to-plist "1y") '(:years 1)))
  (should (equal (org-window-habit-string-duration-to-plist "2Y") '(:years 2))))

(ert-deftest owh-test-string-duration-to-plist-hours ()
  "Test parsing hour durations."
  (should (equal (org-window-habit-string-duration-to-plist "12h") '(:hours 12)))
  (should (equal (org-window-habit-string-duration-to-plist "24H") '(:hours 24))))

(ert-deftest owh-test-string-duration-to-plist-nil ()
  "Test parsing nil returns default."
  (should (equal (org-window-habit-string-duration-to-plist nil :default '(:days 1))
                 '(:days 1)))
  (should (null (org-window-habit-string-duration-to-plist nil))))

(ert-deftest owh-test-string-duration-to-plist-raw-plist ()
  "Test parsing already-plist strings."
  (should (equal (org-window-habit-string-duration-to-plist "(:days 3)")
                 '(:days 3)))
  (should (equal (org-window-habit-string-duration-to-plist "(:months 2 :days 5)")
                 '(:months 2 :days 5))))

;;; Time Normalization Tests

(ert-deftest owh-test-normalize-time-to-days ()
  "Test normalizing time to day boundaries."
  (let* ((input (owh-test-make-time 2024 1 15 14 30 45))
         (result (org-window-habit-normalize-time-to-duration input '(:days 1))))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 15 0 0 0)))))

(ert-deftest owh-test-normalize-time-to-hours ()
  "Test normalizing time to hour boundaries."
  (let* ((input (owh-test-make-time 2024 1 15 14 30 45))
         (result (org-window-habit-normalize-time-to-duration input '(:hours 1))))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 15 14 0 0)))))

(ert-deftest owh-test-normalize-time-to-months ()
  "Test normalizing time to month boundaries."
  (let* ((input (owh-test-make-time 2024 3 15 14 30 45))
         (result (org-window-habit-normalize-time-to-duration input '(:months 1))))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 3 1 0 0 0)))))

(ert-deftest owh-test-normalize-time-to-multiple-months ()
  "Test that multi-month periods start in January."
  (cl-loop for (month n expected) in '((1 3 1) (2 3 1) (3 3 1) (4 3 4) (12 3 10)
                                       (2 2 1) (3 2 3) (12 6 7))
           do (should (owh-test-times-equal-p
                       (org-window-habit-normalize-time-to-duration
                        (owh-test-make-time 2025 month 15 10 0 0)
                        (list :months n))
                       (owh-test-make-time 2025 expected 1 0 0 0)))))

(ert-deftest owh-test-normalize-time-to-days-7-not-week-aligned ()
  "Test that (:days 7) normalizes to day-of-month, NOT week boundaries.
This documents current behavior: '(:days 7)' aligns based on day-of-month
arithmetic, not day-of-week. Use (:weeks 1) for true week alignment.
Jan 15, 2024 is a Monday. With (:days 7), we get Jan 9 (Tuesday), not Jan 15 (Monday)."
  (let* ((input (owh-test-make-time 2024 1 15 14 30 45))  ; Monday
         (result (org-window-habit-normalize-time-to-duration input '(:days 7))))
    ;; Current behavior: aligned-day = 15 - (7-1) = 15 - 6 = 9
    ;; Jan 9, 2024 is a Tuesday, not a Monday
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 9 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-monday-default ()
  "Test that (:weeks 1) aligns to Monday by default.
Jan 15, 2024 is a Monday. Jan 17 is Wednesday. Jan 21 is Sunday.
All should align to Monday Jan 15, 2024."
  (let* ((monday (owh-test-make-time 2024 1 15 14 30 45))
         (wednesday (owh-test-make-time 2024 1 17 10 0 0))
         (sunday (owh-test-make-time 2024 1 21 23 59 59)))
    ;; All should align to Monday Jan 15, 2024
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration monday '(:weeks 1))
             (owh-test-make-time 2024 1 15 0 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration wednesday '(:weeks 1))
             (owh-test-make-time 2024 1 15 0 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration sunday '(:weeks 1))
             (owh-test-make-time 2024 1 15 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-explicit-monday ()
  "Test that (:weeks 1 :start :monday) aligns to Monday."
  (let* ((wednesday (owh-test-make-time 2024 1 17 10 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration wednesday '(:weeks 1 :start :monday))
             (owh-test-make-time 2024 1 15 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-sunday ()
  "Test that (:weeks 1 :start :sunday) aligns to Sunday.
Jan 17, 2024 is Wednesday. The preceding Sunday is Jan 14, 2024."
  (let* ((wednesday (owh-test-make-time 2024 1 17 10 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration wednesday '(:weeks 1 :start :sunday))
             (owh-test-make-time 2024 1 14 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-saturday ()
  "Test that (:weeks 1 :start :saturday) aligns to Saturday.
Jan 17, 2024 is Wednesday. The preceding Saturday is Jan 13, 2024."
  (let* ((wednesday (owh-test-make-time 2024 1 17 10 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration wednesday '(:weeks 1 :start :saturday))
             (owh-test-make-time 2024 1 13 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-on-start-day ()
  "Test week normalization when already on the start day.
Jan 15, 2024 is Monday. With :start :monday, should return that same Monday."
  (let* ((monday (owh-test-make-time 2024 1 15 14 30 45)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration monday '(:weeks 1 :start :monday))
             (owh-test-make-time 2024 1 15 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-cross-month ()
  "Test week normalization when week spans month boundary.
Feb 1, 2024 is a Thursday. The Monday of that week is Jan 29, 2024."
  (let* ((thursday-feb (owh-test-make-time 2024 2 1 12 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration thursday-feb '(:weeks 1))
             (owh-test-make-time 2024 1 29 0 0 0)))))

(ert-deftest owh-test-normalize-time-weeks-cross-year ()
  "Test week normalization when week spans year boundary.
Jan 3, 2024 is a Wednesday. The Monday of that week is Jan 1, 2024."
  (let* ((wednesday-jan (owh-test-make-time 2024 1 3 12 0 0)))
    (should (owh-test-times-equal-p
             (org-window-habit-normalize-time-to-duration wednesday-jan '(:weeks 1))
             (owh-test-make-time 2024 1 1 0 0 0)))))

(ert-deftest owh-test-keyed-duration-add-weeks ()
  "Test adding weeks to a time."
  (let* ((base (owh-test-make-time 2024 1 15 12 30 45))
         (result (org-window-habit-keyed-duration-add :base-time base :weeks 2)))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 29 12 30 45)))))

(ert-deftest owh-test-keyed-duration-add-plist-weeks ()
  "Test adding weeks via plist."
  (let* ((base (owh-test-make-time 2024 1 15))
         (result (org-window-habit-keyed-duration-add-plist base '(:weeks 1))))
    (should (owh-test-times-equal-p result (owh-test-make-time 2024 1 22)))))

;;; DST (Daylight Saving Time) Tests
;;;
;;; These tests verify that assessment intervals work correctly across DST
;;; transitions. The bug: using 86400 seconds for "one day" fails because
;;; DST transition days are 23 or 25 hours, not 24.

(ert-deftest owh-test-dst-assessment-preserves-time-of-day ()
  "Test that assessment intervals preserve time-of-day across DST transitions.

US Pacific time DST 2024:
- March 10: clocks spring forward 2am->3am (day is 23 hours)
- November 3: clocks fall back 2am->1am (day is 25 hours)

If we anchor at noon on March 9 (before DST) and compute the assessment
for March 11 (after DST), the assessment should still start at noon,
not 1pm (which would happen if we naively add 86400 seconds per day).

This test will only detect the bug when run in a DST-aware timezone."
  ;; Set timezone to US Pacific for this test
  (let ((original-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/Los_Angeles")
          ;; Force Emacs to pick up the new timezone
          (set-time-zone-rule "America/Los_Angeles")

          (let* (;; Anchor at noon on March 9, 2024 (last day before DST starts)
                 (anchor-time (encode-time 0 0 12 9 3 2024))
                 ;; Query for March 11, 2024 (first full day in DST)
                 (query-time (encode-time 0 0 12 11 3 2024))
                 ;; Get assessment start using the anchoring function
                 (assessment-start
                  (org-window-habit-get-anchored-assessment-start
                   anchor-time query-time '(:days 1)))
                 ;; Decode to check the hour
                 (decoded (decode-time assessment-start))
                 (hour (nth 2 decoded))
                 (day (nth 3 decoded)))
            ;; The assessment for March 11 should start at NOON on March 11
            ;; Not at 1pm (13:00) which would happen with buggy 86400-second math
            (should (= day 11))
            (should (= hour 12))))
      ;; Restore original timezone
      (if original-tz
          (setenv "TZ" original-tz)
        (setenv "TZ" nil))
      (set-time-zone-rule original-tz))))

(ert-deftest owh-test-dst-fall-back-assessment-preserves-time-of-day ()
  "Test assessment intervals across DST 'fall back' transition.

November 3, 2024: clocks fall back 2am->1am (day is 25 hours)

If we anchor at noon on November 2 and compute the assessment for
November 4, it should still be at noon, not 11am."
  (let ((original-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/Los_Angeles")
          (set-time-zone-rule "America/Los_Angeles")

          (let* (;; Anchor at noon on November 2, 2024 (day before fall back)
                 (anchor-time (encode-time 0 0 12 2 11 2024))
                 ;; Query for November 4, 2024 (day after fall back)
                 (query-time (encode-time 0 0 12 4 11 2024))
                 (assessment-start
                  (org-window-habit-get-anchored-assessment-start
                   anchor-time query-time '(:days 1)))
                 (decoded (decode-time assessment-start))
                 (hour (nth 2 decoded))
                 (day (nth 3 decoded)))
            ;; Should be noon on November 4, not 11am
            (should (= day 4))
            (should (= hour 12))))
      (if original-tz
          (setenv "TZ" original-tz)
        (setenv "TZ" nil))
      (set-time-zone-rule original-tz))))

(ert-deftest owh-test-dst-completion-counted-for-correct-date ()
  "Test that completions near midnight are counted for the correct date.

Bug scenario: User completes habit at 00:00 on Jan 26.
Due to DST-induced anchor drift, the assessment for Jan 26 might start
at 23:00 on Jan 25, causing the 00:00 completion to be counted for Jan 25.

This test verifies that a completion logged at 00:00 on a given date
is counted for that date's assessment, not the previous day's.

Note: The anchor is at midnight because that's what org-window-habit-finalize-instance
does when normalizing the start-time for daily habits."
  (let ((original-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/Los_Angeles")
          (set-time-zone-rule "America/Los_Angeles")

          (let* (;; Habit started in summer (PDT) - normalized to midnight
                 ;; This simulates what org-window-habit-normalize-time-to-duration does
                 (habit-start (encode-time 0 0 0 16 8 2023))  ; Aug 16, 2023 midnight
                 ;; Completion at midnight on Jan 26, 2026 (PST - winter)
                 (completion-time (encode-time 0 0 0 26 1 2026))
                 ;; Reference time for computing assessment
                 (reference-time (encode-time 0 0 12 26 1 2026))
                 ;; Get assessment window for Jan 26
                 (assessment-start
                  (org-window-habit-get-anchored-assessment-start
                   habit-start reference-time '(:days 1)))
                 (assessment-end
                  (org-window-habit-keyed-duration-add-plist
                   assessment-start '(:days 1)))
                 (decoded-start (decode-time assessment-start)))
            ;; The assessment for Jan 26 should start on Jan 26, not Jan 25
            (should (= (nth 3 decoded-start) 26))
            ;; The completion at 00:00 Jan 26 should fall within the Jan 26 assessment
            ;; i.e., completion-time >= assessment-start
            (should (or (time-equal-p completion-time assessment-start)
                        (time-less-p assessment-start completion-time)))
            ;; And completion-time < assessment-end
            (should (time-less-p completion-time assessment-end))))
      (if original-tz
          (setenv "TZ" original-tz)
        (setenv "TZ" nil))
      (set-time-zone-rule original-tz))))

(provide 'org-window-habit-time-test)
;;; org-window-habit-time-test.el ends here
