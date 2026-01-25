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
  "Test org-window-habit-entry-p returns non-nil when both properties exist."
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


;;; ==========================================================================
;;; Scenario-Based Tests: Real-World Habit Configurations
;;; ==========================================================================

;;; Helper for creating test habits with done-times

(defun owh-test-make-habit (window-specs done-times &optional assessment-interval max-reps)
  "Create a habit for testing with WINDOW-SPECS and DONE-TIMES.
ASSESSMENT-INTERVAL defaults to (:days 1).
MAX-REPS defaults to 1."
  (make-instance 'org-window-habit
                 :window-specs window-specs
                 :assessment-interval (or assessment-interval '(:days 1))
                 :max-repetitions-per-interval (or max-reps 1)
                 :done-times (vconcat (sort (copy-sequence done-times)
                                            (lambda (a b) (time-less-p b a))))
                 :start-time nil))

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
;;; Logbook Entry Order Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-fix-logbook-order-after-backdated-completion ()
  "Test that logbook order is fixed after org inserts a backdated entry at top.
When org inserts a log entry at the top (its default behavior), and that entry
has a timestamp older than the existing first entry, our fix should move it
to the correct sorted position."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert "DEADLINE: <2024-01-20 Sat .+1d>\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      ;; Simulate org inserting a backdated entry at the TOP (the bug)
      ;; This Jan 12 entry is WRONGLY placed before Jan 15
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-12 Fri 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      ;; Verify the initial (wrong) state - Jan 12 is before Jan 15
      (let ((initial-times (save-excursion
                             (org-window-habit-parse-logbook))))
        (should (= (length initial-times) 4))
        ;; First entry is Jan 12 (wrong - should be Jan 15)
        (should (time-equal-p (nth 2 (nth 0 initial-times))
                              (owh-test-make-time 2024 1 12 10 0 0))))

      ;; Now call our fix function
      (goto-char (point-min))
      (org-window-habit-fix-logbook-order)

      ;; Parse the logbook again and verify correct order
      (goto-char (point-min))
      (let ((new-times (save-excursion
                         (org-window-habit-parse-logbook))))
        ;; Should still have 4 entries
        (should (= (length new-times) 4))
        ;; Verify they are now in descending chronological order
        ;; Entry 0: Jan 15 (most recent)
        (should (time-equal-p (nth 2 (nth 0 new-times))
                              (owh-test-make-time 2024 1 15 10 0 0)))
        ;; Entry 1: Jan 12 (moved to correct position)
        (should (time-equal-p (nth 2 (nth 1 new-times))
                              (owh-test-make-time 2024 1 12 10 0 0)))
        ;; Entry 2: Jan 10
        (should (time-equal-p (nth 2 (nth 2 new-times))
                              (owh-test-make-time 2024 1 10 10 0 0)))
        ;; Entry 3: Jan 5 (earliest)
        (should (time-equal-p (nth 2 (nth 3 new-times))
                              (owh-test-make-time 2024 1 5 10 0 0)))))))

(ert-deftest owh-test-fix-logbook-order-already-sorted ()
  "Test that fix does nothing when logbook is already sorted."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-05 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-single-entry ()
  "Test that fix handles single entry logbook."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-empty-drawer ()
  "Test that fix handles empty logbook drawer."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((before-content (buffer-string)))
        (org-window-habit-fix-logbook-order)
        ;; Buffer should be unchanged
        (should (string= (buffer-string) before-content))))))

(ert-deftest owh-test-fix-logbook-order-entry-should-go-to-end ()
  "Test fixing when new entry is older than all existing entries."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      ;; Org inserted Jan 1 at top, but it should be at end
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-01 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-10 Wed 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (org-window-habit-fix-logbook-order)

      (goto-char (point-min))
      (let ((times (save-excursion (org-window-habit-parse-logbook))))
        (should (= (length times) 3))
        ;; Jan 15 should be first
        (should (time-equal-p (nth 2 (nth 0 times))
                              (owh-test-make-time 2024 1 15 10 0 0)))
        ;; Jan 10 should be second
        (should (time-equal-p (nth 2 (nth 1 times))
                              (owh-test-make-time 2024 1 10 10 0 0)))
        ;; Jan 1 should be last (moved from top)
        (should (time-equal-p (nth 2 (nth 2 times))
                              (owh-test-make-time 2024 1 1 10 0 0)))))))

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
      (insert ":RESET_TIME: [2024-01-15 Mon 00:00]\n")
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

(provide 'org-window-habit-test)
;;; org-window-habit-test.el ends here
