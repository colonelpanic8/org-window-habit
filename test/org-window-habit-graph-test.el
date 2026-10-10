;;; org-window-habit-graph-test.el --- Graph tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Graph tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-window-habit)
(require 'owh-test-helpers)

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

;;; Graph Building Tests

(ert-deftest owh-test-make-graph-string ()
  "Test graph string creation."
  (let ((graph-info '((?x face1) (?y face2) (?z face3))))
    (should (= (length (org-window-habit-make-graph-string graph-info)) 3))))

(ert-deftest owh-test-make-graph-string-non-ascii-glyphs ()
  "Non-ASCII glyphs such as the default ✓ and ☐ must render with their faces."
  (let* ((graph-info `((,org-window-habit-completed-glyph face1)
                       (?\s face2)
                       (,org-window-habit-completion-needed-today-glyph face3)))
         (graph (org-window-habit-make-graph-string graph-info)))
    (should (equal (substring-no-properties graph)
                   (string org-window-habit-completed-glyph ?\s
                           org-window-habit-completion-needed-today-glyph)))
    (should (equal (mapcar (lambda (i) (get-text-property i 'face graph)) '(0 1 2))
                   '(face1 face2 face3)))
    (should (get-text-property 0 'org-window-habit-graph graph))))

(ert-deftest owh-test-build-graph-marks-required-interval-as-of-now ()
  "The present interval shows a needed completion relative to NOW."
  (let* ((now (owh-test-make-time 2025 3 8 12 0 0))
         (habit (org-window-habit-create-instance-from-config
                 "(:window-specs ((:duration (:days 7) :repetitions 1)))"
                 (vector (owh-test-make-time 2025 3 1 10 0 0))
                 now))
         (graph (org-window-habit-make-graph-string
                 (org-window-habit-build-graph habit now))))
    (should (string-match "|\\(.\\)" graph))
    (should (eq (string-to-char (match-string 1 graph))
                org-window-habit-completion-needed-today-glyph))))

(ert-deftest owh-test-graph-time-honors-extend-today-until ()
  "Day-based habit graphs treat early-morning hours as the previous day."
  (let ((org-extend-today-until 4)
        (daily (owh-test-make-habit
                (list (make-instance 'org-window-habit-window-spec
                                     :duration '(:days 7) :repetitions 1))
                (list (owh-test-make-time 2025 3 1 10 0 0))))
        (hourly (owh-test-make-habit
                 (list (make-instance 'org-window-habit-window-spec
                                      :duration '(:hours 8) :repetitions 1))
                 (list (owh-test-make-time 2025 3 1 10 0 0))
                 '(:hours 1))))
    (cl-letf (((symbol-function 'current-time)
               (lambda () (owh-test-make-time 2025 3 9 2 0 0))))
      (should (time-equal-p (org-window-habit-graph-time daily)
                            (owh-test-make-time 2025 3 8 22 0 0)))
      (should (time-equal-p (org-window-habit-graph-time hourly)
                            (owh-test-make-time 2025 3 9 2 0 0))))))

(ert-deftest owh-test-insert-consistency-graphs-without-completions ()
  "Habits without completions still get a graph, without echo area noise."
  (let* ((habit (owh-test-make-habit
                 (list (make-instance 'org-window-habit-window-spec
                                      :duration '(:days 7) :repetitions 1))
                 nil))
         (org-habit-graph-column 10)
         (messages nil))
    (with-temp-buffer
      (insert (propertize "TODO Habit" 'org-habit-p habit) "\n")
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (push args messages))))
        (org-window-habit-insert-consistency-graphs))
      (goto-char (point-min))
      (should (text-property-any (point-min) (point-max)
                                 'org-window-habit-graph t))
      (should-not messages))))

(provide 'org-window-habit-graph-test)
;;; org-window-habit-graph-test.el ends here
