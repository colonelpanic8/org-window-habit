;;; org-window-habit-meta-test.el --- Meta-evaluation tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Meta-evaluation tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; Meta-Evaluation Tests
;;; ==========================================================================

(ert-deftest owh-test-weighted-geometric-mean-basic ()
  "Test weighted geometric mean calculation."
  ;; Equal weights, scores of 0.5 and 0.5 -> geometric mean = 0.5
  (let ((pairs '((0.5 . 1.0) (0.5 . 1.0))))
    (should (< (abs (- (org-window-habit-weighted-geometric-mean pairs) 0.5)) 0.001)))
  ;; Equal weights, scores of 1.0 and 1.0 -> 1.0
  (let ((pairs '((1.0 . 1.0) (1.0 . 1.0))))
    (should (< (abs (- (org-window-habit-weighted-geometric-mean pairs) 1.0)) 0.001)))
  ;; Score of 0 should result in 0 (harsh penalty)
  (let ((pairs '((0.0 . 1.0) (1.0 . 1.0))))
    (should (= (org-window-habit-weighted-geometric-mean pairs) 0.0)))
  ;; Empty list returns 1.0
  (should (= (org-window-habit-weighted-geometric-mean '()) 1.0)))

(ert-deftest owh-test-weighted-geometric-mean-weights ()
  "Test that weights affect the geometric mean properly."
  ;; With weight 2 on 0.25 and weight 1 on 1.0:
  ;; (0.25^2 * 1.0^1)^(1/3) = (0.0625)^(1/3) ≈ 0.397
  (let ((pairs '((0.25 . 2.0) (1.0 . 1.0))))
    (should (< (abs (- (org-window-habit-weighted-geometric-mean pairs) 0.397)) 0.01))))

(ert-deftest owh-test-weighted-average-basic ()
  "Test weighted average calculation."
  ;; Equal weights: (0.4 + 0.6) / 2 = 0.5
  (let ((pairs '((0.4 . 1.0) (0.6 . 1.0))))
    (should (< (abs (- (org-window-habit-weighted-average pairs) 0.5)) 0.001)))
  ;; With weights: (0.4*2 + 0.8*1) / (2+1) = 1.6/3 ≈ 0.533
  (let ((pairs '((0.4 . 2.0) (0.8 . 1.0))))
    (should (< (abs (- (org-window-habit-weighted-average pairs) 0.533)) 0.01)))
  ;; Empty list returns 1.0
  (should (= (org-window-habit-weighted-average '()) 1.0)))

(ert-deftest owh-test-get-weight-from-config ()
  "Test extracting weight from config."
  ;; Config with explicit weight
  (let ((configs '((:window-specs ((:duration (:days 7) :repetitions 3)) :weight 2.5))))
    (should (= (org-window-habit-get-weight-from-config configs) 2.5)))
  ;; Config without weight uses default
  (let ((configs '((:window-specs ((:duration (:days 7) :repetitions 3)))))
        (org-window-habit-default-weight 1.0))
    (should (= (org-window-habit-get-weight-from-config configs) 1.0)))
  ;; Nil configs uses default
  (let ((org-window-habit-default-weight 1.0))
    (should (= (org-window-habit-get-weight-from-config nil) 1.0))))

(ert-deftest owh-test-meta-evaluate-empty ()
  "Test meta-evaluate with no habits."
  (let ((result (org-window-habit-meta-evaluate '() nil (current-time))))
    (should (= (cdr (assoc "score" result)) 1.0))
    (should (= (cdr (assoc "habitCount" result)) 0))
    (should (null (cdr (assoc "habits" result))))))

(ert-deftest owh-test-meta-evaluate-single-habit ()
  "Test meta-evaluate with a single habit entry."
  (let ((temp-file (make-temp-file "owh-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* TODO Test habit
:PROPERTIES:
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :weight 2.0)
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2024-01-15 Mon 10:00]
- State \"DONE\" from \"TODO\" [2024-01-14 Sun 10:00]
:END:
"))
          (with-current-buffer (find-file-noselect temp-file)
            (let* ((markers (org-window-habit-collect-habits-in-buffer))
                   (query-time (owh-test-make-time 2024 1 15 12 0 0))
                   (result (org-window-habit-meta-evaluate markers nil query-time)))
              ;; Verify structure
              (should (= (cdr (assoc "habitCount" result)) 1))
              (should (assoc "score" result))
              (should (assoc "evaluationTime" result))
              (should (assoc "aggregationFn" result))
              (let ((habit-info (car (cdr (assoc "habits" result)))))
                (should (string= (cdr (assoc "title" habit-info)) "Test habit"))
                (should (= (cdr (assoc "weight" habit-info)) 2.0))
                ;; Verify conformingRatio is a number between 0 and 1
                (let ((ratio (cdr (assoc "conformingRatio" habit-info))))
                  (should (numberp ratio))
                  (should (>= ratio 0.0))
                  (should (<= ratio 1.0)))
                ;; Verify windowSpecsStatus is included
                (should (assoc "windowSpecsStatus" habit-info))))))
      (delete-file temp-file))))

(ert-deftest owh-test-meta-evaluate-weighted-aggregation ()
  "Test that weights affect the meta-score properly."
  (let ((temp-file (make-temp-file "owh-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* TODO High weight habit
:PROPERTIES:
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 2)) :weight 3.0)
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2024-01-15 Mon 10:00]
- State \"DONE\" from \"TODO\" [2024-01-14 Sun 10:00]
:END:

* TODO Low weight habit
:PROPERTIES:
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 4)) :weight 1.0)
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2024-01-15 Mon 10:00]
:END:
"))
          (with-current-buffer (find-file-noselect temp-file)
            (let* ((markers (org-window-habit-collect-habits-in-buffer))
                   (query-time (owh-test-make-time 2024 1 15 12 0 0))
                   ;; Use weighted average for easier verification
                   (result (org-window-habit-meta-evaluate
                            markers
                            #'org-window-habit-weighted-average
                            query-time)))
              ;; Verify we got both habits
              (should (= (cdr (assoc "habitCount" result)) 2))
              ;; Verify the aggregation function was used
              (should (eq (cdr (assoc "aggregationFn" result))
                          #'org-window-habit-weighted-average))
              ;; Verify score is a number
              (let ((score (cdr (assoc "score" result))))
                (should (numberp score))
                (should (>= score 0.0))
                (should (<= score 1.0)))
              ;; Verify both habits have different weights
              (let* ((habits (cdr (assoc "habits" result)))
                     (weights (mapcar (lambda (h) (cdr (assoc "weight" h))) habits)))
                (should (member 3.0 weights))
                (should (member 1.0 weights))))))
      (delete-file temp-file))))

(ert-deftest owh-test-meta-score-series ()
  "Test time series generation for graphing."
  ;; Test with programmatically created data
  (let* ((score-weight-pairs '((0.8 . 1.0) (0.6 . 1.0)))
         ;; Geometric mean of 0.8 and 0.6 = sqrt(0.48) ≈ 0.693
         (expected-score (org-window-habit-weighted-geometric-mean score-weight-pairs)))
    (should (< (abs (- expected-score 0.693)) 0.01))))

(ert-deftest owh-test-config-weight-parsing ()
  "Test that :weight is parsed from CONFIG property."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3)) :weight 2.5)"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      (should (= (plist-get config :weight) 2.5)))))

(ert-deftest owh-test-current-streak-weekly-five-of-seven ()
  "Count consecutive conforming weeks for a 5-of-7 weekly habit."
  (let* ((done-times
          (vector
           ;; Current week, Jan 22-29: conforming.
           (owh-test-make-time 2024 1 26 10 0 0)
           (owh-test-make-time 2024 1 25 10 0 0)
           (owh-test-make-time 2024 1 24 10 0 0)
           (owh-test-make-time 2024 1 23 10 0 0)
           (owh-test-make-time 2024 1 22 10 0 0)
           ;; Previous week, Jan 15-22: conforming.
           (owh-test-make-time 2024 1 19 10 0 0)
           (owh-test-make-time 2024 1 18 10 0 0)
           (owh-test-make-time 2024 1 17 10 0 0)
           (owh-test-make-time 2024 1 16 10 0 0)
           (owh-test-make-time 2024 1 15 10 0 0)
           ;; Week of Jan 8-15: not conforming.
           (owh-test-make-time 2024 1 12 10 0 0)
           (owh-test-make-time 2024 1 11 10 0 0)
           (owh-test-make-time 2024 1 10 10 0 0)
           (owh-test-make-time 2024 1 9 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs
                               (list
                                (make-instance
                                 'org-window-habit-window-spec
                                 :duration '(:weeks 1 :start :monday)
                                 :repetitions 5))
                               :assessment-interval
                               '(:weeks 1 :start :monday)
                               :max-repetitions-per-interval 5
                               :done-times done-times
                               :start-time
                               (owh-test-make-time 2024 1 8 0 0 0)))
         (query-time (owh-test-make-time 2024 1 26 12 0 0)))
    (should (= (org-window-habit-current-streak habit query-time) 2))))

(ert-deftest owh-test-current-streak-status-data ()
  "Expose current streak in status data."
  (let* ((done-times
          (vector
           (owh-test-make-time 2024 1 16 10 0 0)
           (owh-test-make-time 2024 1 15 10 0 0)))
         (habit (make-instance 'org-window-habit
                               :window-specs
                               (list
                                (make-instance
                                 'org-window-habit-window-spec
                                 :duration '(:days 1)
                                 :repetitions 1))
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :start-time
                               (owh-test-make-time 2024 1 15 0 0 0)))
         (status (org-window-habit-get-window-specs-status
                  habit (owh-test-make-time 2024 1 16 12 0 0))))
    (should (= (cdr (assoc "conformingStreak" status)) 2))))

(ert-deftest owh-test-current-streak-no-completions ()
  "Habits without completions have no current streak."
  (let ((habit (make-instance 'org-window-habit
                              :window-specs
                              (list
                               (make-instance
                                'org-window-habit-window-spec
                                :duration '(:days 1)
                                :repetitions 1))
                              :assessment-interval '(:days 1)
                              :done-times (vector)
                              :start-time
                              (owh-test-make-time 2024 1 15 0 0 0))))
    (should (= (org-window-habit-current-streak
                habit (owh-test-make-time 2024 1 16 12 0 0))
               0))))

(provide 'org-window-habit-meta-test)
;;; org-window-habit-meta-test.el ends here
