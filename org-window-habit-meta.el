;;; org-window-habit-meta.el --- Meta-evaluation across habits -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Aggregate scores across multiple habits for meta-evaluation.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-window-habit-time)
(require 'org-window-habit-core)

;; Forward declarations
(declare-function org-window-habit-entry-p "org-window-habit")


;;; Customization

(defcustom org-window-habit-meta-aggregation-fn
  #'org-window-habit-weighted-geometric-mean
  "Function to aggregate habit scores into a meta-score.
The function receives a list of (score . weight) pairs and should
return a single float representing the aggregate score.
Built-in options:
  - `org-window-habit-weighted-geometric-mean' (default)
  - `org-window-habit-weighted-average'"
  :group 'org-window-habit
  :type 'function)

(defcustom org-window-habit-default-weight 1.0
  "Default weight for habits without a :weight key in their config."
  :group 'org-window-habit
  :type 'float)


;;; Aggregation functions

(defun org-window-habit-weighted-geometric-mean (score-weight-pairs)
  "Compute weighted geometric mean from SCORE-WEIGHT-PAIRS.
SCORE-WEIGHT-PAIRS is a list of (score . weight) cons cells.
Returns product(score_i ^ weight_i) ^ (1 / sum(weight_i)).
Scores of 0 result in a 0 aggregate (harsh penalty for failure)."
  (if (null score-weight-pairs)
      1.0
    (let ((total-weight (cl-loop for pair in score-weight-pairs
                                 sum (cdr pair)))
          (log-sum 0.0)
          (has-zero nil))
      (if (zerop total-weight)
          1.0
        (dolist (pair score-weight-pairs)
          (let ((score (car pair))
                (weight (cdr pair)))
            (if (<= score 0)
                (setq has-zero t)
              (setq log-sum (+ log-sum (* weight (log score)))))))
        (if has-zero
            0.0
          (exp (/ log-sum total-weight)))))))

(defun org-window-habit-weighted-average (score-weight-pairs)
  "Compute weighted average from SCORE-WEIGHT-PAIRS.
SCORE-WEIGHT-PAIRS is a list of (score . weight) cons cells.
Returns sum(score_i * weight_i) / sum(weight_i)."
  (if (null score-weight-pairs)
      1.0
    (let ((total-weight (cl-loop for pair in score-weight-pairs
                                 sum (cdr pair)))
          (weighted-sum (cl-loop for pair in score-weight-pairs
                                 sum (* (car pair) (cdr pair)))))
      (if (zerop total-weight)
          1.0
        (/ weighted-sum total-weight)))))


;;; Weight extraction

(defun org-window-habit-get-weight-from-config (configs)
  "Extract :weight from CONFIGS, default `org-window-habit-default-weight'.
CONFIGS is a list of parsed config plists (from a habit's configs slot)."
  (if configs
      (or (plist-get (car configs) :weight)
          org-window-habit-default-weight)
    org-window-habit-default-weight))


;;; Habit collection

(defun org-window-habit-collect-habits-in-buffer ()
  "Collect all window-habit entries in the current buffer.
Returns a list of markers pointing to habit headings."
  (let ((habits '()))
    (org-map-entries
     (lambda ()
       (when (org-window-habit-entry-p)
         (push (point-marker) habits))))
    (nreverse habits)))

(defun org-window-habit-collect-habits-in-agenda ()
  "Collect all window-habit entries across agenda files.
Returns a list of markers pointing to habit headings."
  (let ((habits '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when (org-window-habit-entry-p)
                (push (point-marker) habits))))))))
    (nreverse habits)))


;;; Meta-evaluation

(defun org-window-habit-meta-evaluate (markers &optional aggregation-fn time)
  "Evaluate habits at MARKERS and return aggregated meta-score data.
MARKERS is a list of markers pointing to habit entries.
AGGREGATION-FN is the function to combine scores (defaults to
`org-window-habit-meta-aggregation-fn').
TIME is the evaluation time (defaults to current time).

Returns an alist with:
  - \"score\": the final aggregated score (0.0-1.0+)
  - \"habitCount\": number of habits evaluated
  - \"aggregationFn\": the function used for aggregation
  - \"evaluationTime\": timestamp of evaluation
  - \"habits\": list of per-habit data including marker, title, weight,
    conformingRatio, and full windowSpecsStatus"
  (setq aggregation-fn (or aggregation-fn org-window-habit-meta-aggregation-fn))
  (setq time (or time (current-time)))
  (let ((habit-data '())
        (score-weight-pairs '()))
    ;; Collect data from each habit
    (dolist (marker markers)
      (when (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (when (org-window-habit-entry-p)
              (let* ((title (org-get-heading t t t t))
                     (habit (org-window-habit-create-instance-from-heading-at-point))
                     (status (org-window-habit-get-window-specs-status habit time))
                     (conforming-ratio (cdr (assoc "aggregatedConformingRatio" status)))
                     (weight (org-window-habit-get-weight-from-config (oref habit configs))))
                (push `(("marker" . ,marker)
                        ("title" . ,title)
                        ("weight" . ,weight)
                        ("conformingRatio" . ,conforming-ratio)
                        ("windowSpecsStatus" . ,status))
                      habit-data)
                (push (cons conforming-ratio weight) score-weight-pairs)))))))
    ;; Compute aggregate score
    (let ((score (if score-weight-pairs
                     (funcall aggregation-fn (nreverse score-weight-pairs))
                   1.0)))
      `(("score" . ,score)
        ("habitCount" . ,(length habit-data))
        ("aggregationFn" . ,aggregation-fn)
        ("evaluationTime" . ,time)
        ("habits" . ,(nreverse habit-data))))))

(defun org-window-habit-meta-score (&optional aggregation-fn time)
  "Compute meta-score across all habits in agenda files.
AGGREGATION-FN is the function to combine scores (defaults to
`org-window-habit-meta-aggregation-fn').
TIME is the evaluation time (defaults to current time).

This is a convenience wrapper that collects all agenda habits
and calls `org-window-habit-meta-evaluate'."
  (let ((markers (org-window-habit-collect-habits-in-agenda)))
    (org-window-habit-meta-evaluate markers aggregation-fn time)))


;;; Time-range evaluation

(defun org-window-habit-meta-evaluate-range (markers start-time end-time
                                                      &optional step aggregation-fn)
  "Evaluate habits at MARKERS over a time range.
START-TIME and END-TIME define the range to evaluate.
STEP is a duration plist for sampling interval (defaults to (:days 1)).
AGGREGATION-FN is the function to combine scores.

Returns an alist with:
  - \"startTime\": start of range
  - \"endTime\": end of range
  - \"step\": the step duration used
  - \"snapshots\": list of evaluation snapshots, each containing
    \"time\", \"score\", and \"habits\" data"
  (setq step (or step '(:days 1)))
  (setq aggregation-fn (or aggregation-fn org-window-habit-meta-aggregation-fn))
  (let ((snapshots '())
        (current-time start-time))
    (while (not (time-less-p end-time current-time))
      (let ((result (org-window-habit-meta-evaluate markers aggregation-fn current-time)))
        (push `(("time" . ,current-time)
                ("score" . ,(cdr (assoc "score" result)))
                ("habits" . ,(cdr (assoc "habits" result))))
              snapshots))
      (setq current-time (org-window-habit-keyed-duration-add-plist current-time step)))
    `(("startTime" . ,start-time)
      ("endTime" . ,end-time)
      ("step" . ,step)
      ("aggregationFn" . ,aggregation-fn)
      ("snapshots" . ,(nreverse snapshots)))))

(defun org-window-habit-meta-score-range (start-time end-time &optional step aggregation-fn)
  "Compute meta-scores over a time range for all agenda habits.
START-TIME and END-TIME define the range.
STEP is a duration plist (defaults to (:days 1)).
AGGREGATION-FN is the score aggregation function.

This is a convenience wrapper that collects all agenda habits
and calls `org-window-habit-meta-evaluate-range'."
  (let ((markers (org-window-habit-collect-habits-in-agenda)))
    (org-window-habit-meta-evaluate-range markers start-time end-time step aggregation-fn)))

(defun org-window-habit-meta-score-series (start-time end-time &optional step aggregation-fn)
  "Compute a compact time series of meta-scores for graphing.
START-TIME and END-TIME define the range.
STEP is a duration plist (defaults to (:days 1)).
AGGREGATION-FN is the score aggregation function.

Returns a list of (time . score) pairs suitable for plotting."
  (let* ((result (org-window-habit-meta-score-range
                  start-time end-time step aggregation-fn))
         (snapshots (cdr (assoc "snapshots" result))))
    (mapcar (lambda (snapshot)
              (cons (cdr (assoc "time" snapshot))
                    (cdr (assoc "score" snapshot))))
            snapshots)))

(provide 'org-window-habit-meta)
;;; org-window-habit-meta.el ends here
