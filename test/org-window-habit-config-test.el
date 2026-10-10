;;; org-window-habit-config-test.el --- Versioned config tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Versioned config tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Date Parsing Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-parse-config-date-inactive-timestamp ()
  "Test parsing inactive org timestamp [YYYY-MM-DD]."
  (let ((result (org-window-habit-parse-config-date "[2025-06-01]")))
    (should result)
    (should (owh-test-times-equal-p result (owh-test-make-time 2025 6 1 0 0 0)))))

(ert-deftest owh-test-parse-config-date-inactive-timestamp-with-day ()
  "Test parsing inactive org timestamp with day name [YYYY-MM-DD Day]."
  (let ((result (org-window-habit-parse-config-date "[2025-06-01 Sun]")))
    (should result)
    (should (owh-test-times-equal-p result (owh-test-make-time 2025 6 1 0 0 0)))))

(ert-deftest owh-test-parse-config-date-active-timestamp ()
  "Test parsing active org timestamp <YYYY-MM-DD>."
  (let ((result (org-window-habit-parse-config-date "<2025-06-01>")))
    (should result)
    (should (owh-test-times-equal-p result (owh-test-make-time 2025 6 1 0 0 0)))))

(ert-deftest owh-test-parse-config-date-plain-string ()
  "Test parsing plain date string \"YYYY-MM-DD\"."
  (let ((result (org-window-habit-parse-config-date "2025-06-01")))
    (should result)
    (should (owh-test-times-equal-p result (owh-test-make-time 2025 6 1 0 0 0)))))

(ert-deftest owh-test-parse-config-date-nil ()
  "Test parsing nil returns nil."
  (should (null (org-window-habit-parse-config-date nil))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Config Parsing Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-parse-config-single-config ()
  "Test parsing a single config (bare plist, not in list)."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3))
                     :assessment-interval (:days 1))"))
    (let ((configs (org-window-habit-parse-config config-str)))
      ;; Should return a list with one config
      (should (= (length configs) 1))
      (let ((config (car configs)))
        ;; Should have window-specs
        (should (plist-get config :window-specs))
        ;; Should have assessment-interval
        (should (equal (plist-get config :assessment-interval) '(:days 1)))
        ;; Should NOT have :from or :until (implicitly unbounded)
        (should (null (plist-get config :from)))
        (should (null (plist-get config :until)))))))

(ert-deftest owh-test-parse-config-versioned-two-configs ()
  "Test parsing versioned configs (list of plists)."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let ((configs (org-window-habit-parse-config config-str)))
      ;; Should return 2 configs
      (should (= (length configs) 2))
      ;; First config (current)
      (let ((first (nth 0 configs)))
        (should (equal (plist-get (car (plist-get first :window-specs)) :repetitions) 3))
        ;; :from should be implicitly the :until of the second config
        (should (owh-test-times-equal-p (plist-get first :from)
                                        (owh-test-make-time 2025 6 1 0 0 0)))
        ;; :until should be nil (unbounded future)
        (should (null (plist-get first :until))))
      ;; Second config (older)
      (let ((second (nth 1 configs)))
        (should (equal (plist-get (car (plist-get second :window-specs)) :repetitions) 2))
        ;; :until should be parsed
        (should (owh-test-times-equal-p (plist-get second :until)
                                        (owh-test-make-time 2025 6 1 0 0 0)))
        ;; :from should be nil (unbounded past)
        (should (null (plist-get second :from)))))))

(ert-deftest owh-test-parse-config-bounded-older-config ()
  "Test that a newer config chains from an older config with both bounds."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-03-01\"
                      :from \"2025-01-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let ((configs (org-window-habit-parse-config config-str)))
      (should (= (length configs) 2))
      (should (owh-test-times-equal-p (plist-get (nth 0 configs) :from)
                                      (owh-test-make-time 2025 3 1 0 0 0)))
      (let ((second (nth 1 configs)))
        (should (owh-test-times-equal-p (plist-get second :from)
                                        (owh-test-make-time 2025 1 1 0 0 0)))
        (should (owh-test-times-equal-p (plist-get second :until)
                                        (owh-test-make-time 2025 3 1 0 0 0))))
      (should (eq (org-window-habit-get-config-for-time
                   configs (owh-test-make-time 2025 2 1 12 0 0))
                  (nth 1 configs)))
      (should (null (org-window-habit-get-config-for-time
                     configs (owh-test-make-time 2024 12 1 12 0 0)))))))

(ert-deftest owh-test-parse-config-explicit-from-on-last ()
  "Test that explicit :from on last config acts like RESET_TIME."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3))
                      :from \"2024-06-01\"))"))
    (let ((configs (org-window-habit-parse-config config-str)))
      (should (= (length configs) 1))
      (let ((config (car configs)))
        ;; Explicit :from on the only/last config
        (should (owh-test-times-equal-p (plist-get config :from)
                                        (owh-test-make-time 2024 6 1 0 0 0)))))))

(ert-deftest owh-test-parse-config-missing-window-specs-error ()
  "Test that missing :window-specs causes an error."
  (let ((config-str "(:assessment-interval (:days 1))"))
    (should-error (org-window-habit-parse-config config-str))))

(ert-deftest owh-test-parse-config-wrong-temporal-order-error ()
  "Test that wrong temporal order causes an error."
  ;; First config should be most recent, but here older is first
  (let ((config-str "((:until \"2025-01-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2)))
                     (:window-specs ((:duration (:days 7) :repetitions 3))))"))
    ;; The second config has no :until meaning it's current,
    ;; but it comes AFTER a config with :until, which is wrong order
    (should-error (org-window-habit-parse-config config-str))))

(ert-deftest owh-test-parse-config-three-configs-chained ()
  "Test parsing three configs with proper implicit chaining."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-01-01\"
                      :window-specs ((:duration (:days 7) :repetitions 1))))"))
    (let ((configs (org-window-habit-parse-config config-str)))
      (should (= (length configs) 3))
      ;; First config: :from = 2025-06-01 (from second's :until), :until = nil
      (let ((first (nth 0 configs)))
        (should (owh-test-times-equal-p (plist-get first :from)
                                        (owh-test-make-time 2025 6 1 0 0 0)))
        (should (null (plist-get first :until))))
      ;; Second config: :from = 2025-01-01 (from third's :until), :until = 2025-06-01
      (let ((second (nth 1 configs)))
        (should (owh-test-times-equal-p (plist-get second :from)
                                        (owh-test-make-time 2025 1 1 0 0 0)))
        (should (owh-test-times-equal-p (plist-get second :until)
                                        (owh-test-make-time 2025 6 1 0 0 0))))
      ;; Third config: :from = nil (unbounded past), :until = 2025-01-01
      (let ((third (nth 2 configs)))
        (should (null (plist-get third :from)))
        (should (owh-test-times-equal-p (plist-get third :until)
                                        (owh-test-make-time 2025 1 1 0 0 0)))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Config Resolution Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-get-config-for-time-current ()
  "Test resolving config for time in current (first) config."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query after 2025-06-01
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2025 7 15 0 0 0))))
      (should result)
      ;; Should return first config (3 repetitions)
      (should (equal (plist-get (car (plist-get result :window-specs)) :repetitions) 3)))))

(ert-deftest owh-test-get-config-for-time-older ()
  "Test resolving config for time in older config."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query before 2025-06-01
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2025 3 15 0 0 0))))
      (should result)
      ;; Should return second config (2 repetitions)
      (should (equal (plist-get (car (plist-get result :window-specs)) :repetitions) 2)))))

(ert-deftest owh-test-get-config-for-time-in-gap ()
  "Test resolving config for time in a gap returns nil."
  (let ((config-str "((:from \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-03-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query in gap between 2025-03-01 and 2025-06-01
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2025 4 15 0 0 0))))
      (should (null result)))))

(ert-deftest owh-test-get-config-for-time-before-explicit-from ()
  "Test resolving config for time before explicit :from on last config returns nil."
  (let ((config-str "((:from \"2024-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 3))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query before 2024-06-01
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2024 3 15 0 0 0))))
      (should (null result)))))

(ert-deftest owh-test-get-config-for-time-at-boundary ()
  "Test resolving config exactly at boundary (newer config wins - inclusive start)."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query exactly at 2025-06-01 00:00:00
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2025 6 1 0 0 0))))
      (should result)
      ;; Boundary belongs to newer config (inclusive start, exclusive end)
      (should (equal (plist-get (car (plist-get result :window-specs)) :repetitions) 3)))))

(ert-deftest owh-test-get-config-for-time-far-past-no-from ()
  "Test resolving config for time far in past with no :from on oldest returns oldest."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 3)))
                     (:until \"2025-06-01\"
                      :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query far in the past (2020)
           (result (org-window-habit-get-config-for-time
                    configs (owh-test-make-time 2020 1 1 0 0 0))))
      (should result)
      ;; Should return oldest config (2 repetitions)
      (should (equal (plist-get (car (plist-get result :window-specs)) :repetitions) 2)))))

(ert-deftest owh-test-bounded-single-config-is-inactive-after-until ()
  "A bounded single config must not produce current requirement data after :until."
  (let* ((config-str
          "(:until \"2025-03-01\" :window-specs ((:duration (:days 7) :repetitions 2)))")
         (active-time (owh-test-make-time 2025 2 28 12))
         (inactive-time (owh-test-make-time 2025 3 1))
         (habit (org-window-habit-create-instance-from-config
                 config-str
                 (vector (owh-test-make-time 2025 2 27 10)
                         (owh-test-make-time 2025 2 26 10))
                 active-time))
         (window (org-window-habit-get-assessment-window
                  (car (oref habit window-specs)) inactive-time))
         (graph-result
          (org-window-habit-default-graph-assessment-fn
           0.0 1.0 0 'present habit window)))
    (should habit)
    (should-not (org-window-habit-active-at-time-p habit inactive-time))
    (should-not (org-window-habit-for-time habit inactive-time))
    (should-not (org-window-habit-create-instance-from-config
                 config-str [] inactive-time))
    (should-not (org-window-habit-get-window-specs-status
                 habit inactive-time))
    (should-not (org-window-habit-current-streak habit inactive-time))
    (should-not (org-window-habit-get-next-required-interval
                 habit inactive-time))
    (should-not (org-window-habit-build-graph habit inactive-time))
    (should (equal (org-window-habit-make-graph-display-string
                    habit inactive-time)
                   ""))
    (should-not (eq (car (nth 1 graph-result))
                    org-window-habit-completion-needed-today-glyph))))

(ert-deftest owh-test-versioned-config-gap-is-inactive ()
  "Historical and resumed versions stay evaluable while their gap is inactive."
  (let* ((config-str
          "((:from \"2025-06-01\" :window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2025-03-01\" :window-specs ((:duration (:days 7) :repetitions 2))))")
         (historical-time (owh-test-make-time 2025 2 28 12))
         (gap-time (owh-test-make-time 2025 4 15 12))
         (resumed-time (owh-test-make-time 2025 6 15 12))
         (habit (org-window-habit-create-instance-from-config
                 config-str
                 (vector (owh-test-make-time 2025 6 14 10)
                         (owh-test-make-time 2025 2 27 10)
                         (owh-test-make-time 2025 2 26 10))
                 resumed-time))
         (historical-status
          (org-window-habit-get-window-specs-status habit historical-time))
         (resumed-status
          (org-window-habit-get-window-specs-status habit resumed-time)))
    (should (= (cdr (assoc "targetRepetitions"
                           (car (cdr (assoc "windowSpecsStatus"
                                            historical-status)))))
               2))
    (should (>= (cdr (assoc "aggregatedConformingRatio"
                            historical-status))
                0.99))
    (should-not (org-window-habit-active-at-time-p habit gap-time))
    (should-not (org-window-habit-get-window-specs-status habit gap-time))
    (should-not (org-window-habit-get-next-required-interval habit gap-time))
    (should (= (cdr (assoc "targetRepetitions"
                           (car (cdr (assoc "windowSpecsStatus"
                                            resumed-status)))))
               5))))

(ert-deftest owh-test-runtime-config-boundaries-are-half-open ()
  "Runtime selection treats :from as inclusive and :until as exclusive."
  (let* ((boundary (owh-test-make-time 2025 6 1))
         (before-boundary (time-subtract boundary (seconds-to-time 1)))
         (config-str
          "((:from \"2025-06-01\" :window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2025-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))")
         (before (org-window-habit-create-instance-from-config
                  config-str [] before-boundary))
         (at (org-window-habit-create-instance-from-config
              config-str [] boundary)))
    (should (= (oref (car (oref before window-specs)) target-repetitions) 2))
    (should (= (oref (car (oref at window-specs)) target-repetitions) 5))))

(ert-deftest owh-test-current-active-version-retains-requirement-computations ()
  "An unbounded current version remains active and produces requirement data."
  (let* ((time (owh-test-make-time 2025 7 15 12))
         (habit
          (org-window-habit-create-instance-from-config
           "((:from \"2025-06-01\" :window-specs ((:duration (:days 7) :repetitions 3))) (:until \"2025-06-01\" :window-specs ((:duration (:days 7) :repetitions 1))))"
           (vector (owh-test-make-time 2025 7 14 10))
           time))
         (status (org-window-habit-get-window-specs-status habit time)))
    (should (org-window-habit-active-at-time-p habit time))
    (should (numberp (cdr (assoc "aggregatedConformingRatio" status))))
    (should (org-window-habit-get-next-required-interval habit time))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Instance Creation and Backwards Compatibility Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-create-instance-from-config-property ()
  "Test creating habit instance from CONFIG property."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :assessment-interval (:days 1))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Should have created a habit
        (should habit)
        ;; Should have window specs
        (should (oref habit window-specs))
        ;; Should have 3 repetitions required
        (should (= (oref (car (oref habit window-specs)) target-repetitions) 3))
        ;; Should have configs slot populated
        (should (oref habit configs))))))

(ert-deftest owh-test-create-instance-from-config-property-with-aggregation-fn ()
  "Test creating a habit instance from CONFIG with a custom aggregation function."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (goto-char (point-min))
      (org-entry-put
       nil "CONFIG"
       "(:window-specs ((:duration (:days 2) :repetitions 1 :value 1.0) (:duration (:days 4) :repetitions 2 :value 1.0) (:duration (:days 8) :repetitions 4 :value 1.0)) :assessment-interval (:days 1) :aggregation-fn org-window-habit-weighted-average-aggregation-fn :reschedule-interval (:days 2))")
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        (should habit)
        (should (eq (oref habit aggregation-fn)
                    'org-window-habit-weighted-average-aggregation-fn))
        (should (equal (oref habit reschedule-interval) '(:days 2)))
        (should (= (length (oref habit window-specs)) 3))))))

(ert-deftest owh-test-create-instance-backwards-compatible ()
  "Test that old scattered properties still work without CONFIG."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 5\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Should have created a habit
        (should habit)
        ;; Should have 5 repetitions required (from old property)
        (should (= (oref (car (oref habit window-specs)) target-repetitions) 5))))))

(ert-deftest owh-test-config-takes-precedence-over-old-properties ()
  "Test that CONFIG property takes precedence over old scattered properties."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :assessment-interval (:days 1))\n")
      (insert ":WINDOW_DURATION: 14d\n")  ; Should be ignored
      (insert ":REPETITIONS_REQUIRED: 10\n")  ; Should be ignored
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Should use CONFIG values, not old properties
        (should (= (oref (car (oref habit window-specs)) target-repetitions) 3))))))

(ert-deftest owh-test-versioned-config-from-property ()
  "Test creating habit with versioned config from property."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: ((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3))))\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-07-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-03-15 Fri 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Should have configs populated
        (should (oref habit configs))
        (should (= (length (oref habit configs)) 2))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Migration Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-migrate-simple-habit ()
  "Test migrating habit with WINDOW_DURATION + REPETITIONS_REQUIRED."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-window-habit-migrate-to-config)
      ;; Should now have CONFIG property
      (should (org-entry-get nil "CONFIG"))
      ;; Parse and verify
      (let ((configs (org-window-habit-parse-config (org-entry-get nil "CONFIG"))))
        (should (= (length configs) 1))
        (should (equal (plist-get (car (plist-get (car configs) :window-specs)) :duration)
                       '(:days 7)))
        (should (= (plist-get (car (plist-get (car configs) :window-specs)) :repetitions) 3))))))

(ert-deftest owh-test-migrate-habit-with-reset-time ()
  "Test that RESET_TIME is converted to :from on config."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":RESET_TIME: [2024-06-01]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-window-habit-migrate-to-config)
      (let ((configs (org-window-habit-parse-config (org-entry-get nil "CONFIG"))))
        ;; Should have :from set from RESET_TIME
        (should (owh-test-times-equal-p (plist-get (car configs) :from)
                                        (owh-test-make-time 2024 6 1 0 0 0)))))))

(ert-deftest owh-test-migrate-habit-with-window-specs ()
  "Test migrating habit with WINDOW_SPECS property preserves all properties."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_SPECS: ((:duration (:days 7) :repetitions 3) (:duration (:days 30) :repetitions 10))\n")
      (insert ":ASSESSMENT_INTERVAL: (:days 2)\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-window-habit-migrate-to-config)
      (let ((configs (org-window-habit-parse-config (org-entry-get nil "CONFIG"))))
        (should (= (length configs) 1))
        ;; Should preserve multiple window specs
        (should (= (length (plist-get (car configs) :window-specs)) 2))
        ;; Should preserve assessment-interval
        (should (equal (plist-get (car configs) :assessment-interval) '(:days 2)))))))

(ert-deftest owh-test-migrate-habit-with-all-properties ()
  "Test migrating habit with all optional properties."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":ASSESSMENT_INTERVAL: (:days 2)\n")
      (insert ":RESCHEDULE_INTERVAL: (:days 1)\n")
      (insert ":MAX_REPETITIONS_PER_INTERVAL: 2\n")
      (insert ":ONLY_DAYS: (:monday :wednesday :friday)\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-window-habit-migrate-to-config)
      (let ((configs (org-window-habit-parse-config (org-entry-get nil "CONFIG"))))
        (let ((config (car configs)))
          (should (equal (plist-get config :assessment-interval) '(:days 2)))
          (should (equal (plist-get config :reschedule-interval) '(:days 1)))
          (should (= (plist-get config :max-reps-per-interval) 2))
          (should (equal (plist-get config :only-days) '(:monday :wednesday :friday))))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Completion Counting with Config Changes Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-completion-count-all-in-current-config ()
  "Test completion counting when all completions are in current config.
The configs slot stores versioned config data for reference, but basic
completion counting remains unchanged."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (done-times (vector
                        (owh-test-make-time 2024 7 15 10 0 0)
                        (owh-test-make-time 2024 7 14 10 0 0)
                        (owh-test-make-time 2024 7 13 10 0 0)))
           (habit (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 5))
                                 :assessment-interval '(:days 1)
                                 :done-times done-times
                                 :configs configs
                                 :start-time (owh-test-make-time 2024 7 1 0 0 0)))
           ;; Count completions in a window entirely within current config
           (count (org-window-habit-get-completion-count
                   habit
                   (owh-test-make-time 2024 7 10 0 0 0)
                   (owh-test-make-time 2024 7 16 0 0 0))))
      ;; Should count all 3 completions
      (should (= count 3)))))

(ert-deftest owh-test-completion-count-spanning-config-boundary ()
  "Test completion counting when completions span a config boundary.
The standard completion count works regardless of config - configs affect
the target repetitions for conforming ratio, not raw completion counting."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Completions spanning the June 1 boundary
           (done-times (vector
                        (owh-test-make-time 2024 6 5 10 0 0)   ; After boundary
                        (owh-test-make-time 2024 6 3 10 0 0)   ; After boundary
                        (owh-test-make-time 2024 5 31 10 0 0)  ; Before boundary (last day of May)
                        (owh-test-make-time 2024 5 30 10 0 0))) ; Before boundary
           (habit (make-instance 'org-window-habit
                                 :window-specs (list (make-instance 'org-window-habit-window-spec
                                                                    :duration '(:days 7)
                                                                    :repetitions 5))
                                 :assessment-interval '(:days 1)
                                 :done-times done-times
                                 :configs configs
                                 :start-time (owh-test-make-time 2024 5 25 0 0 0))))
      ;; Window: May 29 to June 6
      ;; Completions in window: June 5, June 3, May 31, May 30 = 4 completions
      (let ((count (org-window-habit-get-completion-count
                    habit
                    (owh-test-make-time 2024 5 29 0 0 0)
                    (owh-test-make-time 2024 6 6 0 0 0))))
        ;; Should count all 4 completions in the window
        (should (= count 4))))))

(ert-deftest owh-test-completion-count-in-gap-ignored ()
  "Test that completions during a gap can be identified via config resolution.
When a completion time falls in a gap (no active config), `org-window-habit-get-config-for-time'
returns nil, allowing callers to filter those completions."
  (let ((config-str "((:from \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-03-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Completion times: after gap, IN gap, before gap
           (time-after-gap (owh-test-make-time 2024 6 5 10 0 0))
           (time-in-gap (owh-test-make-time 2024 4 15 10 0 0))
           (time-before-gap (owh-test-make-time 2024 2 15 10 0 0)))
      ;; After gap - should resolve to first config
      (should (org-window-habit-get-config-for-time configs time-after-gap))
      ;; In gap - should return nil
      (should (null (org-window-habit-get-config-for-time configs time-in-gap)))
      ;; Before gap - should resolve to second config
      (should (org-window-habit-get-config-for-time configs time-before-gap)))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Interactive Command Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-insert-config-change-single-to-versioned ()
  "Test that inserting config change converts single config to versioned."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))\n")
      (insert ":END:\n")
      (goto-char (point-min))
      ;; Simulate inserting config change effective 2025-06-01
      (org-window-habit-insert-config-change-at-date (owh-test-make-time 2025 6 1))
      ;; Should now be versioned
      (let ((configs (org-window-habit-parse-config (org-entry-get nil "CONFIG"))))
        (should (= (length configs) 2))
        ;; First config should have :from of 2025-06-01
        (should (owh-test-times-equal-p (plist-get (nth 0 configs) :from)
                                        (owh-test-make-time 2025 6 1 0 0 0)))
        ;; Second config should have :until of 2025-06-01
        (should (owh-test-times-equal-p (plist-get (nth 1 configs) :until)
                                        (owh-test-make-time 2025 6 1 0 0 0)))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Complete Config Parameter Tests
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-config-contains-all-parameters ()
  "Test that a config can contain all habit parameters."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3))
                     :assessment-interval (:days 2)
                     :reschedule-assessment-interval (:days 1)
                     :reschedule-interval (:days 1)
                     :reschedule-threshold 0.8
                     :max-reps-per-interval 2
                     :only-days (:monday :wednesday :friday)
                     :reschedule-days (:monday :friday)
                     :aggregation-fn org-window-habit-default-aggregation-fn)"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      (should (equal (plist-get config :assessment-interval) '(:days 2)))
      (should (equal (plist-get config :reschedule-assessment-interval) '(:days 1)))
      (should (equal (plist-get config :reschedule-interval) '(:days 1)))
      (should (= (plist-get config :reschedule-threshold) 0.8))
      (should (= (plist-get config :max-reps-per-interval) 2))
      (should (equal (plist-get config :only-days) '(:monday :wednesday :friday)))
      (should (equal (plist-get config :reschedule-days) '(:monday :friday)))
      (should (eq (plist-get config :aggregation-fn) 'org-window-habit-default-aggregation-fn)))))

(ert-deftest owh-test-versioned-configs-different-assessment-intervals ()
  "Test versioned configs where assessment-interval changes over time."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :assessment-interval (:days 1)) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3)) :assessment-interval (:days 7)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (current-config (nth 0 configs))
           (old-config (nth 1 configs)))
      ;; Current config: daily assessment
      (should (equal (plist-get current-config :assessment-interval) '(:days 1)))
      ;; Old config: weekly assessment
      (should (equal (plist-get old-config :assessment-interval) '(:days 7))))))

(ert-deftest owh-test-versioned-configs-different-max-reps ()
  "Test versioned configs where max-reps-per-interval changes."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :max-reps-per-interval 2) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3)) :max-reps-per-interval 1))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (current-config (nth 0 configs))
           (old-config (nth 1 configs)))
      ;; Current: 2 reps per interval count
      (should (= (plist-get current-config :max-reps-per-interval) 2))
      ;; Old: 1 rep per interval counts
      (should (= (plist-get old-config :max-reps-per-interval) 1)))))

(ert-deftest owh-test-versioned-configs-different-only-days ()
  "Test versioned configs where only-days restriction changes."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :only-days (:monday :tuesday :wednesday :thursday :friday)) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2)) :only-days (:monday :wednesday :friday)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (current-config (nth 0 configs))
           (old-config (nth 1 configs)))
      ;; Current: weekdays
      (should (equal (plist-get current-config :only-days)
                     '(:monday :tuesday :wednesday :thursday :friday)))
      ;; Old: MWF only
      (should (equal (plist-get old-config :only-days)
                     '(:monday :wednesday :friday))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Get Config Parameters for Time
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-get-assessment-interval-for-time ()
  "Test getting assessment-interval from config active at a given time."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :assessment-interval (:days 1)) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3)) :assessment-interval (:days 7)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Time after June 1 - should get daily
           (config-after (org-window-habit-get-config-for-time
                          configs (owh-test-make-time 2024 7 15)))
           ;; Time before June 1 - should get weekly
           (config-before (org-window-habit-get-config-for-time
                           configs (owh-test-make-time 2024 3 15))))
      (should (equal (plist-get config-after :assessment-interval) '(:days 1)))
      (should (equal (plist-get config-before :assessment-interval) '(:days 7))))))

(ert-deftest owh-test-get-max-reps-for-time ()
  "Test getting max-reps-per-interval from config active at a given time."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :max-reps-per-interval 3) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3)) :max-reps-per-interval 1))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config-after (org-window-habit-get-config-for-time
                          configs (owh-test-make-time 2024 7 15)))
           (config-before (org-window-habit-get-config-for-time
                           configs (owh-test-make-time 2024 3 15))))
      (should (= (plist-get config-after :max-reps-per-interval) 3))
      (should (= (plist-get config-before :max-reps-per-interval) 1)))))

(ert-deftest owh-test-get-only-days-for-time ()
  "Test getting only-days from config active at a given time."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5)) :only-days (:monday :wednesday :friday)) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config-after (org-window-habit-get-config-for-time
                          configs (owh-test-make-time 2024 7 15)))
           (config-before (org-window-habit-get-config-for-time
                           configs (owh-test-make-time 2024 3 15))))
      ;; After: has only-days restriction
      (should (equal (plist-get config-after :only-days) '(:monday :wednesday :friday)))
      ;; Before: no only-days (nil)
      (should (null (plist-get config-before :only-days))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Default Values for Missing Keys
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-config-defaults-assessment-interval ()
  "Test that missing assessment-interval defaults correctly."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      ;; No :assessment-interval in config
      (should (null (plist-get config :assessment-interval)))
      ;; Helper function should return default
      (should (equal (org-window-habit-config-get-assessment-interval config)
                     '(:days 1))))))

(ert-deftest owh-test-config-defaults-max-reps ()
  "Test that missing max-reps-per-interval defaults to 1."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      ;; No :max-reps-per-interval in config
      (should (null (plist-get config :max-reps-per-interval)))
      ;; Helper function should return default
      (should (= (org-window-habit-config-get-max-reps-per-interval config) 1)))))

(ert-deftest owh-test-config-defaults-reschedule-threshold ()
  "Test that missing reschedule-threshold defaults to 1.0."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3)))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      (should (null (plist-get config :reschedule-threshold)))
      (should (= (org-window-habit-config-get-reschedule-threshold config) 1.0)))))

(ert-deftest owh-test-config-defaults-reschedule-assessment-interval ()
  "Missing reschedule-assessment-interval defaults to one day."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3))
                     :assessment-interval (:weeks 1 :start :monday)
                     :reschedule-interval (:days 2))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      (should (null (plist-get config :reschedule-assessment-interval)))
      (should
       (equal
        (org-window-habit-config-get-reschedule-assessment-interval config)
        '(:days 1))))))

(ert-deftest owh-test-config-defaults-reschedule-interval ()
  "Missing reschedule-interval defaults to one day."
  (let ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3))
                     :assessment-interval (:weeks 1 :start :monday))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (config (car configs)))
      (should (null (plist-get config :reschedule-interval)))
      (should (equal (org-window-habit-config-get-reschedule-interval config)
                     '(:days 1)))
      (should
       (equal
        (org-window-habit-config-get-reschedule-assessment-interval config)
        '(:days 1))))))

(ert-deftest owh-test-create-instance-from-config-default-rescheduling ()
  "CONFIG habits default to daily rescheduling even with weekly assessment."
  (let* ((config-str "(:window-specs ((:duration (:weeks 1 :start :monday)
                                      :repetitions 5))
                     :assessment-interval (:weeks 1 :start :monday)
                     :max-reps-per-interval 5)")
         (habit (org-window-habit-create-instance-from-config config-str [])))
    (should (equal (oref habit assessment-interval)
                   '(:weeks 1 :start :monday)))
    (should (equal (oref habit reschedule-interval) '(:days 1)))
    (should (equal (oref habit reschedule-assessment-interval)
                   '(:days 1)))))

(ert-deftest owh-test-create-instance-from-config-reschedule-options ()
  "CONFIG reschedule options are passed into habit instances."
  (let* ((config-str "(:window-specs ((:duration (:days 7) :repetitions 3))
                     :assessment-interval (:weeks 1 :start :monday)
                     :reschedule-assessment-interval (:days 1)
                     :reschedule-interval (:days 1)
                     :reschedule-threshold 0.8
                     :reschedule-days (:monday :wednesday :friday))")
         (habit (org-window-habit-create-instance-from-config config-str [])))
    (should (equal (oref habit assessment-interval)
                   '(:weeks 1 :start :monday)))
    (should (equal (oref habit reschedule-assessment-interval)
                   '(:days 1)))
    (should (equal (oref habit reschedule-interval) '(:days 1)))
    (should (= (oref habit reschedule-threshold) 0.8))
    (should (equal (oref habit reschedule-days)
                   '(:monday :wednesday :friday)))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Scattered Properties Conversion
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-scattered-to-config-all-properties ()
  "Test that all scattered properties convert to config format."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":ASSESSMENT_INTERVAL: (:days 2)\n")
      (insert ":RESCHEDULE_ASSESSMENT_INTERVAL: (:days 1)\n")
      (insert ":RESCHEDULE_INTERVAL: (:days 1)\n")
      (insert ":RESCHEDULE_THRESHOLD: 0.8\n")
      (insert ":MAX_REPETITIONS_PER_INTERVAL: 2\n")
      (insert ":ONLY_DAYS: (:monday :wednesday :friday)\n")
      (insert ":RESCHEDULE_DAYS: (:monday :friday)\n")
      (insert ":RESET_TIME: [2024-06-01]\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-07-15 Mon 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Should have configs populated
        (should (oref habit configs))
        (let ((config (car (oref habit configs))))
          ;; All properties should be in the config
          (should (plist-get config :window-specs))
          (should (equal (plist-get config :assessment-interval) '(:days 2)))
          (should (equal (plist-get config :reschedule-assessment-interval)
                         '(:days 1)))
          (should (equal (plist-get config :reschedule-interval) '(:days 1)))
          (should (= (plist-get config :reschedule-threshold) 0.8))
          (should (= (plist-get config :max-reps-per-interval) 2))
          (should (equal (plist-get config :only-days) '(:monday :wednesday :friday)))
          (should (equal (plist-get config :reschedule-days)
                         '(:monday :friday)))
          ;; RESET_TIME should become :from
          (should (owh-test-times-equal-p (plist-get config :from)
                                          (owh-test-make-time 2024 6 1))))))))

(ert-deftest owh-test-scattered-to-config-minimal ()
  "Test that minimal scattered properties work."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_DURATION: 7d\n")
      (insert ":REPETITIONS_REQUIRED: 3\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-07-15 Mon 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        (should (oref habit configs))
        (let ((config (car (oref habit configs))))
          ;; Should have window-specs
          (should (plist-get config :window-specs))
          ;; Optional fields should be nil (defaults applied at usage time)
          (should (null (plist-get config :assessment-interval)))
          (should (null (plist-get config :from))))))))

(ert-deftest owh-test-scattered-properties-default-daily-rescheduling ()
  "Scattered properties default to daily rescheduling for weekly assessment."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":WINDOW_SPECS: ((:duration (:weeks 1 :start :monday) :repetitions 5))\n")
      (insert ":ASSESSMENT_INTERVAL: (:weeks 1 :start :monday)\n")
      (insert ":MAX_REPETITIONS_PER_INTERVAL: 5\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        (should (equal (oref habit assessment-interval)
                       '(:weeks 1 :start :monday)))
        (should (equal (oref habit reschedule-interval) '(:days 1)))
        (should (equal (oref habit reschedule-assessment-interval)
                       '(:days 1)))
        (let ((config (car (oref habit configs))))
          (should (null (plist-get config :reschedule-interval)))
          (should (null (plist-get config
                                   :reschedule-assessment-interval))))))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: No Separate reset-time Slot
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-reset-time-derived-from-config ()
  "Test that effective reset time comes from config :from, not separate slot."
  (let ((org-window-habit-property-prefix nil))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n")
      (insert ":PROPERTIES:\n")
      (insert ":CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :from \"2024-06-01\")\n")
      (insert ":END:\n")
      (insert ":LOGBOOK:\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-07-15 Mon 10:00]\n")
      (insert "- State \"DONE\"       from \"TODO\"       [2024-05-15 Wed 10:00]\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (let ((habit (org-window-habit-create-instance-from-heading-at-point)))
        ;; Effective start should be from config :from
        (let ((effective-start (org-window-habit-get-effective-start habit)))
          (should (owh-test-times-equal-p effective-start
                                          (owh-test-make-time 2024 6 1))))
        ;; Completions before :from should not count
        (let ((count (org-window-habit-get-completion-count
                      habit
                      (owh-test-make-time 2024 5 1)
                      (owh-test-make-time 2024 7 20))))
          ;; Only July 15 completion should count, not May 15
          (should (= count 1)))))))

(ert-deftest owh-test-effective-start-from-versioned-config ()
  "Test effective start with versioned configs uses oldest config's :from."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :from \"2024-01-01\" :window-specs ((:duration (:days 7) :repetitions 3))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; The effective start is the :from of the oldest config
           (effective-start (org-window-habit-configs-get-effective-start configs)))
      (should (owh-test-times-equal-p effective-start
                                      (owh-test-make-time 2024 1 1))))))

(ert-deftest owh-test-effective-start-unbounded-past ()
  "Test effective start when oldest config has no :from (unbounded past)."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 3))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           (effective-start (org-window-habit-configs-get-effective-start configs)))
      ;; No :from on oldest config means unbounded - should return nil
      (should (null effective-start)))))

;;; ---------------------------------------------------------------------------
;;; Versioned Config: Historical Evaluation Uses Correct Config
;;; ---------------------------------------------------------------------------

(ert-deftest owh-test-historical-window-specs ()
  "Test that evaluating at historical time uses that time's window-specs."
  (let ((config-str "((:window-specs ((:duration (:days 7) :repetitions 5))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
    (let* ((configs (org-window-habit-parse-config config-str))
           ;; Query current period - need 5 reps
           (current-config (org-window-habit-get-config-for-time
                            configs (owh-test-make-time 2024 7 15)))
           ;; Query old period - needed 2 reps
           (old-config (org-window-habit-get-config-for-time
                        configs (owh-test-make-time 2024 3 15))))
      (should (= (plist-get (car (plist-get current-config :window-specs)) :repetitions) 5))
      (should (= (plist-get (car (plist-get old-config :window-specs)) :repetitions) 2)))))

(ert-deftest owh-test-conforming-ratio-uses-historical-config ()
  "Test that conforming ratio calculation uses config active at evaluation time."
  ;; Config: after June 1 need 4 reps, before June 1 needed 2 reps
  (let* ((done-times (vector
                      ;; 2 completions in late May (before boundary)
                      (owh-test-make-time 2024 5 28 10 0 0)
                      (owh-test-make-time 2024 5 25 10 0 0)))
         (configs (org-window-habit-parse-config
                   "((:window-specs ((:duration (:days 7) :repetitions 4))) (:until \"2024-06-01\" :window-specs ((:duration (:days 7) :repetitions 2))))"))
         (old-config (org-window-habit-get-config-for-time
                      configs (owh-test-make-time 2024 5 28)))
         (old-window-specs (cl-loop for args in (plist-get old-config :window-specs)
                                    collect (apply #'make-instance
                                                   'org-window-habit-window-spec args)))
         (habit (make-instance 'org-window-habit
                               :window-specs old-window-specs
                               :assessment-interval '(:days 1)
                               :done-times done-times
                               :configs configs
                               :start-time (owh-test-make-time 2024 5 20)))
         (iterator (org-window-habit-iterator-from-time
                    (car old-window-specs) (owh-test-make-time 2024 5 28 12 0 0)))
         (ratio (org-window-habit-conforming-ratio iterator)))
    ;; With 2 completions and old config requiring 2, should be at 1.0
    (should (>= ratio 0.99))))

(provide 'org-window-habit-config-test)
;;; org-window-habit-config-test.el ends here
