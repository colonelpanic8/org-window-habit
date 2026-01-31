;;; org-window-habit-graph.el --- Graph display functions -*- lexical-binding: t; -*-

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

;; Graph building and display functions for org-window-habit.

;;; Code:

(require 'cl-lib)
(require 'org-habit)
(require 'org-window-habit-core)

;; Forward declarations for customization variables from main module
(defvar org-window-habit-conforming-color)
(defvar org-window-habit-not-conforming-color)
(defvar org-window-habit-required-completion-foreground-color)
(defvar org-window-habit-non-required-completion-foreground-color)
(defvar org-window-habit-non-conforming-scale)
(defvar org-window-habit-completion-needed-today-glyph)
(defvar org-window-habit-completed-glyph)
(defvar org-window-habit-preceding-intervals)
(defvar org-window-habit-following-days)
(defvar org-window-habit-graph-assessment-fn)


;;; Face creation

(defun org-window-habit-create-face (bg-color foreground-color)
  "Create or return a face with BG-COLOR background and FOREGROUND-COLOR.
Face names are cached based on color values to avoid recreating faces."
  (let* ((bg-name (replace-regexp-in-string "#" "" bg-color))
         (fg-name (replace-regexp-in-string "#" "" foreground-color))
         (face-name (intern (format "org-window-habit-face-bg-%s-fg-%s" bg-name fg-name))))
    (if (facep face-name)
        face-name
      (progn
        (make-face face-name)
        (set-face-attribute face-name nil :background bg-color :foreground foreground-color)
        face-name))))


;;; Color utilities

(defun org-window-habit-rescale-assessment-value (value)
  "Apply non-conforming scale to VALUE if below 1.0.
Values at or above 1.0 (fully conforming) are returned unchanged."
  (if (>= value 1.0) value
    (*  org-window-habit-non-conforming-scale value)))

(defun org-window-habit-lerp-color (color1 color2 proportion)
  "Linearly interpolate between COLOR1 and COLOR2 by PROPORTION.
PROPORTION of 0.0 returns COLOR1, 1.0 returns COLOR2.
Colors should be hex strings like \"#RRGGBB\"."
  (let ((r1 (string-to-number (substring color1 1 3) 16))
        (g1 (string-to-number (substring color1 3 5) 16))
        (b1 (string-to-number (substring color1 5 7) 16))
        (r2 (string-to-number (substring color2 1 3) 16))
        (g2 (string-to-number (substring color2 3 5) 16))
        (b2 (string-to-number (substring color2 5 7) 16)))
    (format "#%02x%02x%02x"
            (round (+ (* (- r2 r1) proportion) r1))
            (round (+ (* (- g2 g1) proportion) g1))
            (round (+ (* (- b2 b1) proportion) b1)))))


;;; Default graph assessment function

(cl-defun org-window-habit-default-graph-assessment-fn
    (without-completion-assessment-value
     with-completion-assessment-value
     completions-in-interval
     current-interval-time-type
     habit
     window)
  "Determine graph character and face for an interval.
WITHOUT-COMPLETION-ASSESSMENT-VALUE is the ratio if no completion today.
WITH-COMPLETION-ASSESSMENT-VALUE is the ratio with today's completions.
COMPLETIONS-IN-INTERVAL is how many completions occurred in this interval.
CURRENT-INTERVAL-TIME-TYPE is \\='past, \\='present, or \\='future.
HABIT and WINDOW provide context for computing next-required-interval.
Returns (character face) or a list of such pairs for the present interval."
  (let* ((with-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            with-completion-assessment-value)))
         (without-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            without-completion-assessment-value)))
         (completion-today-matters
          (< without-completion-assessment-value with-completion-assessment-value))
         (next-required-interval (org-window-habit-get-next-required-interval habit))
         (completion-expected-today
          (org-window-habit-time-falls-in-assessment-interval
           window
           next-required-interval))
         (interval-has-completion (> completions-in-interval 0)))
    (pcase current-interval-time-type
      ('present
       (let* ((character
               (cond
                (interval-has-completion org-window-habit-completed-glyph)
                (completion-expected-today
                 org-window-habit-completion-needed-today-glyph)
                (t ?-)))
              (completion-face
               (org-window-habit-create-face with-completion-color "#000000"))
              (sample-face
               (if interval-has-completion
                   completion-face
                 (org-window-habit-create-face without-completion-color "#000000"))))
         (list
          (list ?| completion-face)
          (list character completion-face)
          (list ?\s sample-face)
          (list ?| completion-face))))
      ('past
       (let* ((bg-color
               (if completion-expected-today
                   without-completion-color
                 with-completion-color))
              (fg-color
               (cond
                (completion-expected-today
                 with-completion-color)
                (completion-today-matters
                 org-window-habit-required-completion-foreground-color)
                (t org-window-habit-non-required-completion-foreground-color)))
              (face (org-window-habit-create-face bg-color fg-color))
              (character
               (cond
                ((> completions-in-interval 0) org-habit-completed-glyph)
                (t ?\s))))
         (list character face)))
      ('future
       (let* ((bg-color
               without-completion-color)
              (fg-color
               "#000000")
              (face (org-window-habit-create-face bg-color fg-color))
              (character
               (cond
                (completion-expected-today
                 org-window-habit-completion-needed-today-glyph)
                (t ?\s))))
         (list character face))))))


;;; Graph building

(cl-defmethod org-window-habit-build-graph ((habit org-window-habit) &optional now)
  "Build the consistency graph data for HABIT as of NOW.
Returns a list of (character face) pairs for each interval:
- Past intervals showing historical conformity
- Present interval showing current status
- Future intervals projecting expected conformity"
  (setq now (or now (current-time)))
  (with-slots
      (assessment-decrement-plist window-specs reschedule-interval
                                  max-repetitions-per-interval start-time aggregation-fn
                                  assessment-interval graph-assessment-fn)
      habit
    (unless graph-assessment-fn
      (setq graph-assessment-fn
            org-window-habit-graph-assessment-fn))
    (cl-destructuring-bind (actual-intervals actual-start-time)
        ;; Find the start time by going back by the assessment interval
        ;; `org-window-habit-preceding-intervals' times, or hitting the habits
        ;; absolute start.
        (cl-loop
         with target-start-time = (org-window-habit-normalize-time-to-duration
                                   now assessment-interval)
         for i from 0 to org-window-habit-preceding-intervals
         while (time-less-p start-time target-start-time)
         do
         (setq target-start-time
               (org-window-habit-keyed-duration-add-plist
                target-start-time
                assessment-decrement-plist))
         finally return (list i target-start-time))
      (nconc
       ;; Add filler if we don't have enough data to fill `org-window-habit-preceding-intervals'.
       (cl-loop for i from 0 to (- org-window-habit-preceding-intervals actual-intervals)
                collect (list ?\s 'default))
       (cl-loop
        with iterators =
        (cl-loop for window-spec in window-specs
                 collect
                 (org-window-habit-iterator-from-time
                  window-spec actual-start-time))
        while (time-less-p (oref (oref (car iterators) window) assessment-end-time) now)
        for
        (_current-assessment-start
         _current-assessment-end
         no-completions-assessment
         with-completions-assessment
         completion-in-interval-count) =
         (org-window-habit-assess-interval-with-and-without-completions
          habit iterators (lambda (x) x))
        collect
        (funcall
         graph-assessment-fn
         no-completions-assessment
         with-completions-assessment
         completion-in-interval-count
         'past
         habit
         (oref (car iterators) window))
        into past-assessments
        do
        (cl-loop for iterator in iterators
                 do (org-window-habit-advance iterator))
        finally
        return
        (cl-destructuring-bind
            (_current-assessment-start
             _current-assessment-end
             no-completions-assessment
             with-completions-assessment
             completion-in-interval-count)
            (org-window-habit-assess-interval-with-and-without-completions
             habit iterators (lambda (_x) max-repetitions-per-interval))
          (nconc past-assessments
                 ;; This is a hack to allow multi character stuff for the current day
                 (org-window-habit-maybe-make-list-of-lists
                  (funcall graph-assessment-fn no-completions-assessment
                           with-completions-assessment
                           completion-in-interval-count 'present habit
                           (oref (car iterators) window)))
                 (cl-loop
                  for i from 1 to org-window-habit-following-days
                  do
                  (cl-loop for iterator in iterators
                           do (org-window-habit-advance iterator))
                  for
                  (_current-assessment-start
                   _current-assessment-end
                   no-completions-assessment
                   with-completions-assessment
                   completion-in-interval-count) =
                   (org-window-habit-assess-interval-with-and-without-completions
                    habit iterators (lambda (_x) 0))
                   collect
                   (funcall
                    graph-assessment-fn
                    no-completions-assessment
                    with-completions-assessment
                    completion-in-interval-count
                    'future
                    habit
                    (oref (car iterators) window))))))))))


;;; Graph rendering

(defun org-window-habit-make-graph-string (graph-info)
  "Convert GRAPH-INFO into a propertized string for display.
GRAPH-INFO is a list of (character face) pairs."
  (let ((graph (make-string (length graph-info) ?\s)))
    (cl-loop for (character face) in graph-info
             for index from 0
             do
             (progn
               (aset graph index character)
               (put-text-property index (1+ index) 'face face graph)))
    graph))

(defun org-window-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks.
If LINE is provided, insert graphs at beggining of line"
  (let ((inhibit-read-only t)
	(buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p))
          (_invisible-prop (get-text-property (point) 'invisible)))
	  (when (and habit
                 (or (org-window-habit-has-any-done-times habit)
                     (progn (message "Skipping habit with no done times") nil)))
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			              (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-window-habit-make-graph-string
          (org-window-habit-build-graph habit))))
      ;; TODO: this should be reintroduced
      ;; Inherit invisible state of hidden entries.
      ;; (when invisible-prop
      ;;   (put-text-property
      ;;    (- (point) org-habit-graph-column) (point)
      ;;    'invisible invisible-prop))))
	  (forward-line))))))

(provide 'org-window-habit-graph)
;;; org-window-habit-graph.el ends here
