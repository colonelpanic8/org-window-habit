;;; org-window-habit-test.el --- Tests for org-window-habit -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-window-habit

;;; Code:

(require 'ert)
(require 'org-window-habit)

;;; Test Helpers

(defun owh-test-make-time (year month day &optional hour minute second)
  "Create an Emacs time value for YEAR MONTH DAY HOUR MINUTE SECOND."
  (encode-time (or second 0) (or minute 0) (or hour 0) day month year))

(defun owh-test-times-equal-p (t1 t2)
  "Check if times T1 and T2 are equal, allowing for minor float differences."
  (< (abs (float-time (time-subtract t1 t2))) 1))

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
  "Test parsing week durations."
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
  "Test that reschedule-interval defaults to assessment-interval."
  (let* ((spec (make-instance 'org-window-habit-window-spec))
         (habit (make-instance 'org-window-habit
                               :window-specs (list spec)
                               :assessment-interval '(:days 2)
                               :reschedule-interval nil
                               :done-times []
                               :start-time nil)))
    (should (equal (oref habit reschedule-interval) '(:days 2)))))

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

;;; Color Function Tests

(ert-deftest owh-test-lerp-color-endpoints ()
  "Test color interpolation at endpoints."
  (should (equal (org-window-habit-lerp-color "#000000" "#ffffff" 0.0) "#000000"))
  (should (equal (org-window-habit-lerp-color "#000000" "#ffffff" 1.0) "#ffffff")))

(ert-deftest owh-test-lerp-color-midpoint ()
  "Test color interpolation at midpoint."
  (let ((result (org-window-habit-lerp-color "#000000" "#ffffff" 0.5)))
    ;; Should be approximately #808080 (gray)
    (should (string-match-p "^#[78][0-9a-f][78][0-9a-f][78][0-9a-f]$" result))))

(ert-deftest owh-test-lerp-color-red-to-green ()
  "Test color interpolation between colors."
  (let ((result (org-window-habit-lerp-color "#ff0000" "#00ff00" 0.5)))
    ;; Red and green at 50% should give yellow-ish
    (should (string-match-p "^#[78][0-9a-f][78][0-9a-f]00$" result))))

(ert-deftest owh-test-rescale-assessment-value-conforming ()
  "Test rescaling conforming values."
  (should (= (org-window-habit-rescale-assessment-value 1.0) 1.0))
  (should (= (org-window-habit-rescale-assessment-value 1.5) 1.5)))

(ert-deftest owh-test-rescale-assessment-value-non-conforming ()
  "Test rescaling non-conforming values."
  (let ((org-window-habit-non-conforming-scale 0.5))
    (should (= (org-window-habit-rescale-assessment-value 0.8) 0.4))
    (should (= (org-window-habit-rescale-assessment-value 0.5) 0.25))))

;;; Face Creation Tests

(ert-deftest owh-test-create-face ()
  "Test that face creation returns a face symbol."
  (let ((face (org-window-habit-create-face "#ff0000" "#00ff00")))
    (should (symbolp face))
    (should (facep face))))

(ert-deftest owh-test-create-face-caching ()
  "Test that same colors return same face."
  (let ((face1 (org-window-habit-create-face "#123456" "#654321"))
        (face2 (org-window-habit-create-face "#123456" "#654321")))
    (should (eq face1 face2))))

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

;;; Graph Building Tests

(ert-deftest owh-test-make-graph-string ()
  "Test graph string creation."
  (let ((graph-info '((?x face1) (?y face2) (?z face3))))
    (should (= (length (org-window-habit-make-graph-string graph-info)) 3))))

(provide 'org-window-habit-test)
;;; org-window-habit-test.el ends here
