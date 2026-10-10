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

(provide 'org-window-habit-graph-test)
;;; org-window-habit-graph-test.el ends here
