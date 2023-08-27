(require 'calendar)
(require 'org)
(require 'cl-lib)

(defun org-window-habit-time-max (&rest args)
  "Return the maximum time value from ARGS."
  (let ((max-time (car args)))
    (dolist (time (cdr args))
      (when (time-less-p max-time time)
        (setq max-time time)))
    max-time))

(defun org-window-habit-negate-plist (plist)
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (push key result)
        (push (- value) result)))
    (nreverse result)))

(defun org-window-habit-duration-proportion (start-time end-time between-time)
  (let* ((full-interval (float-time (time-subtract end-time start-time)))
         (partial-interval (float-time (time-subtract end-time between-time))))
    (/ partial-interval full-interval)))

(cl-defun org-window-habit-keyed-duration-add
    (&key (base-time (current-time))
          (days 0) (months 0) (years 0)
          (hours 0) (minutes 0) (seconds 0))
  (let* ((decoded-base (decode-time base-time))
         (base-year (nth 5 decoded-base))
         (base-month (nth 4 decoded-base))
         (base-day (nth 3 decoded-base))
         (base-absolute (calendar-absolute-from-gregorian (list base-month base-day base-year)))
         (new-absolute (+ base-absolute days))
         (gregorian-result (calendar-gregorian-from-absolute new-absolute))
         (result-year (+ (caddr gregorian-result) years))
         (result-month (+ (car gregorian-result) months)))

    ;; Handle month overflows and underflows
    (while (> result-month 12)
      (setq result-month (- result-month 12)
            result-year (+ result-year 1)))

    (while (< result-month 1)
      (setq result-month (+ result-month 12)
            result-year (- result-year 1)))

    (encode-time (+ seconds (nth 0 decoded-base))
                 (+ minutes (nth 1 decoded-base))
                 (+ hours (nth 2 decoded-base))
                 (cadr gregorian-result)
                 result-month
                 result-year)))

(defun org-window-habit-keyed-duration-add-plist (base-time plist)
  (apply 'org-window-habit-keyed-duration-add :base-time base-time plist))

(defun org-window-habit-string-duration-to-plist (string-value)
  (if (null string-value)
      (list :days 1)
    (let ((read-value (read string-value)))
      (cond
       ((plistp read-value) read-value)
       ((string-match "\\([0-9]+\\)[Yy]" string-value)
        (list :years (string-to-number (match-string 1 string-value))))

       ;; Month pattern
       ((string-match "\\([0-9]+\\)[Mm]" string-value)
        (list :months (string-to-number (match-string 1 string-value))))

       ;; Week pattern
       ((string-match "\\([0-9]+\\)[Ww]" string-value)
        (list :days (* 7 (string-to-number (match-string 1 string-value)))))

       ;; Day pattern
       ((string-match "\\([0-9]+\\)[Dd]" string-value)
        (list :days (string-to-number (match-string 1 string-value))))

       ;; Hour pattern
       ((string-match "\\([0-9]+\\)[Hh]" string-value)
        (list :hours (string-to-number (match-string 1 string-value))))
       (t (list :days read-value))))))

(defun org-window-habit-normalize-time-to-duration (time-value &optional duration-plist alignment-time)
  (let* ((alignment-decoded (decode-time (or alignment-time time-value)))
         (year (nth 5 alignment-decoded))
         (month (nth 4 alignment-decoded))
         (day (nth 3 alignment-decoded))
         (hour (nth 2 alignment-decoded))
         (minute (nth 1 alignment-decoded))
         (second (nth 0 alignment-decoded))
         (smallest-duration-type (car (last duration-plist 2)))
         (smallest-duration-value (cadr (last duration-plist 2))))

    ;; Align time based on the smallest duration type and its value
    (cond
     ((eq smallest-duration-type :seconds)
      (encode-time (* smallest-duration-value (floor second smallest-duration-value)) minute hour day month year))

     ((eq smallest-duration-type :minutes)
      (encode-time 0 (* smallest-duration-value (floor minute smallest-duration-value)) hour day month year))

     ((eq smallest-duration-type :hours)
      (encode-time 0 0 (* smallest-duration-value (floor hour smallest-duration-value)) day month year))

     ((eq smallest-duration-type :days)
      (let* ((aligned-day (- day (1- smallest-duration-value))))
        (encode-time 0 0 0 aligned-day month year)))

     ((eq smallest-duration-type :months)
      (encode-time 0 0 0 1 (* smallest-duration-value (floor month smallest-duration-value)) year))

     ((eq smallest-duration-type :years)
      (let* ((aligned-year (- year (1- smallest-duration-value))))
        (encode-time 0 0 0 1 1 aligned-year)))

     (t time-value))))

(defun org-window-habit-find-aligned-bounding-time (time-value duration-plist aligned-time)
  (let (new-time)
    (while (time-less-p time-value aligned-time)
      (setq new-time
            (org-window-habit-keyed-duration-add-plist aligned-time duration-plist))
      (when (not (time-less-p new-time aligned-time))
        (error "Time did not decrease in alignment attempt"))
      (setq aligned-time new-time)))
  aligned-time)

(defun org-window-habit-logbook-drawer-bounds ()
  (when (re-search-forward org-logbook-drawer-re nil t)
    (list (match-beginning 0) (match-end 0))))

(defun org-window-habit-parse-logbook ()
  (let ((bounds (org-window-habit-logbook-drawer-bounds)))
    (when bounds
      (cl-destructuring-bind (start end) bounds
        (goto-char start)
        (let ((re (org-window-habit-get-logbook-entry-re)))
          (cl-loop while (re-search-forward re end t)
                   collect (list
                            (match-string-no-properties 1)
                            (match-string-no-properties 2)
                            (org-time-string-to-time (match-string-no-properties 3)))))))))

(defun org-window-habit-get-logbook-entry-re (&optional state-regexp)
  (unless state-regexp
    (setq state-regexp (rx alphanumeric)))
  (rx
   (: line-start (* space) "-" (* space))
   "State" (* space) (? "\"") (group (* alphanumeric)) (? "\"")
   (* space) (? (: "from" (* space) (? "\"") (group (* alphanumeric)) (? "\"")))
   (* space)
   (regexp org-ts-regexp-inactive)))

(defclass org-window-habit ()
  ((duration-plist :initarg :duration-plist :initform '(:days 1))
   (assessment-interval :initarg :assessment-interval :initform '(:days 1))
   (repetitions-required :initarg :repetitions-required :initform 1)
   (okay-repetitions-required :initarg :okay-repetitions-required :initform 1)
   (done-times :initarg :done-times :initform nil)
   (window-decrement-plist :initarg :window-decrement-plist :initform nil)))

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an org-window-habit instance from the current org entry."
  (save-excursion
    (let* ((done-times
            (cl-loop for state-change-info in (org-window-habit-parse-logbook)
                     if (member (nth 0 state-change-info) org-done-keywords)
                     collect (nth 2 state-change-info)))
           (done-times-vector (vconcat done-times))
           (window-length (org-window-habit-string-duration-to-plist
                           (org-entry-get nil "WINDOW_DURATION" "1d")))
           (assessment-interval (org-window-habit-string-duration-to-plist
                                 (org-entry-get nil "ASSESMENT_INTERVAL" "1d")))
           (repetitions-required (string-to-number
                                  (or (org-entry-get nil "REPETITIONS_REQUIRED" t) "1")))
           (okay-repetitions-required (string-to-number
                                       (or (org-entry-get nil "OKAY_REPETITIONS_REQUIRED" t) "1"))))
      (make-instance 'org-window-habit
                     :duration-plist window-length
                     :assessment-interval assessment-interval
                     :repetitions-required repetitions-required
                     :okay-repetitions-required okay-repetitions-required
                     :done-times done-times-vector
                     :window-decrement-plist (org-window-habit-negate-plist assessment-interval)))))

(cl-defmethod org-window-habit-get-completion-window-indices
  ((habit org-window-habit) start-time end-time &key (start-index 0) (end-index 0))
  (with-slots (done-times) habit
    ;; Adjust the start-index based on end-time
    (while (and (>= start-index 0) (< start-index (length done-times))
                (not (time-less-p (aref done-times start-index) end-time))) ; exclusive of end-time
      (setq start-index (1+ start-index)))

    ;; Adjust the end-index based on start-time
    (let ((initial-end-index end-index))
      (while (and (>= end-index 0) (< end-index (length done-times))
                  (and (time-less-p start-time (aref done-times end-index)) ; inclusive of start-time
                       (not (time-equal-p start-time (aref done-times end-index)))))
        (setq end-index (1+ end-index)))

      (list start-index end-index))))

;; TODO avoid using current-time
(cl-defmethod org-window-habit-get-windows
  ((window-habit org-window-habit) &key (max-intervals nil))
  (with-slots (duration-plist done-times window-decrement-plist) window-habit
    (let* ((done-times-count (length done-times))
           (earliest-completion (aref done-times (- done-times-count 1))))
      (cl-loop
       with start-index = 0
       with end-index = 0
       with interval-ongoing = t
       with current-window-start =
       (org-window-habit-normalize-time-to-duration (current-time) duration-plist)
       for current-window-end =
       (org-window-habit-keyed-duration-add-plist current-window-start duration-plist)
       for (new-start-index new-end-index) =
       (org-window-habit-get-completion-window-indices
        window-habit current-window-start current-window-end
        :start-index start-index :end-index (or end-index 0))
       for last-start = current-window-start
       do
       (setq start-index new-start-index
             end-index new-end-index)
       when (>= start-index done-times-count)
       return windows
       for effective-start =
       (if (time-less-p earliest-completion current-window-start)
           current-window-start
           (org-window-habit-time-max
            current-window-start
            (org-window-habit-find-aligned-bounding-time earliest-completion
                                                         window-decrement-plist
                                                         current-window-end)))
       collect
       (list current-window-start effective-start current-window-end
             start-index end-index interval-ongoing)
       into windows
       do (setq interval-ongoing nil)
       when (and max-intervals (>= (length windows) max-intervals))
       return windows
       do
       (setq current-window-start
             (org-window-habit-keyed-duration-add-plist
              current-window-start
              window-decrement-plist))
       when (not (time-less-p current-window-start last-start))
       do (error "The window start did not get smaller")))))

(cl-defmethod org-window-habit-advance-window
  ((window-habit org-window-habit) start-time end-time end-index)
  (with-slots (duration-plist done-times window-decrement-plist) window-habit
    ))


(defvar org-window-habit-face-fn 'org-window-habit-default-face-fn)

(defface org-window-habit-conformed-with-completion-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for intervals on which a the user was conforming with their completion but not without it."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-conforming-without-completion-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for intervals for which the user is conforming without any completions."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-conforming-with-completion-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for currently ongoing interval for which the user will only be conforming with a completion"
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-okay-conforming-face
  '((((background light)) (:background "#FF00FF"))
    (((background dark)) (:background "#FF00FF")))
  "Face for interval in which the user is only okay conforming ."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-extreme-not-conforming-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for interval in which the user is not conforming by a large ."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-not-conforming-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for interval in which the user is not conforming."
  :group 'org-window-habit
  :group 'org-faces)

(cl-defun org-window-habit-default-face-fn
    (perfect-repetitions-required
     okay-repetitions-required
     completions-without-interval
     completions-in-interval
     interval-ongoing &key
     (completions-per-interval 1))
    (cond
     ((>= completions-without-interval perfect-repetitions-required)
      'org-window-habit-conforming-without-completion-face)
     ((>= (+ completions-without-interval completions-in-interval) perfect-repetitions-required)
      'org-window-habit-conformed-with-completion-face)
     ((and interval-ongoing
           (>= (+ completions-without-interval completions-per-interval)
               perfect-repetitions-required))
      'org-window-habit-conforming-with-completion-face)
     ((>= (+ completions-without-interval completions-in-interval)
          okay-repetitions-required)
      'org-window-habit-okay-conforming-face)
     (t 'org-window-habit-not-conforming-face)))

(defun org-window-habit-duration-proportion (start-time end-time between-time)
  (let* ((full-interval (float-time (time-subtract end-time start-time)))
         (partial-interval (float-time (time-subtract end-time between-time))))
    (/ partial-interval full-interval)))

(defcustom org-window-habit-preceding-intervals 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(cl-defmethod org-window-habit-build-graph ((habit org-window-habit))
  (with-slots
      (duration-plist repetitions-required okay-repetitions-required window-decrement-plist)
      habit
    (let* ((past-and-present-windows
            (nreverse (org-window-habit-get-windows
                       habit :max-intervals org-window-habit-preceding-intervals)))
           (filler-count (- org-window-habit-preceding-intervals
                            (length past-and-present-windows))))
      (nconc
       (cl-loop for i from 0 to filler-count
                collect (list ?\s 'org-window-habit-conforming-without-completion-face))
       (cl-loop
        for (start-time actual-start-time end-time start-index end-index interval-ongoing)
        in past-and-present-windows
        for duration-proportion =
        (org-window-habit-duration-proportion start-time end-time actual-start-time)
        for scaled-repetitions-required = (* duration-proportion repetitions-required)
        for scaled-okay-repetitions-required = (* duration-proportion okay-repetitions-required)
        for interval-start-time =
        (org-window-habit-keyed-duration-add-plist
         end-time window-decrement-plist)
        for (interval-start-index interval-end-index) =
        (org-window-habit-get-completion-window-indices
         habit interval-start-time end-time
         :start-index start-index :end-index start-index)
        for total-completions = (- end-index start-index)
        for completions-in-interval = (- interval-end-index interval-start-index)
        for completions-outside-interval = (- total-completions completions-in-interval)
        for face =
        (funcall org-window-habit-face-fn
                 scaled-repetitions-required
                 scaled-okay-repetitions-required
                 completions-outside-interval
                 completions-in-interval
                 interval-ongoing)
        for character =
        (cond
         ((>= completions-in-interval 1) org-habit-completed-glyph)
         (interval-ongoing org-habit-today-glyph)
         (t ?\s))
        collect (list character face))))))

(defun org-window-habit-make-graph-string (graph-info)
  (let ((graph (make-string (length graph-info) ?\s)))
    (cl-loop for (character face) in graph-info
             for index from 0
             do
             (progn
               (aset graph index character)
               (put-text-property index (1+ index) 'face face graph)))
    graph))

(define-minor-mode org-window-habit-mode
  "Minor mode that replaces the normal org-habit functionality."
  :lighter nil
  :global t
  :group 'org-window-habit
  :require 'org-window-habit)

(defun org-window-habit-parse-todo ()
  (org-window-habit-create-instance-from-heading-at-point))

(defun org-window-habit-parse-todo-advice (orig &rest args)
  (if org-window-habit-mode
      (org-window-habit-parse-todo)
    (apply orig args)))

(advice-add 'org-habit-parse-todo
            :around 'org-window-habit-parse-todo-advice)

(defun org-window-habit-insert-consistency-graphs-advice (orig &rest args)
  (if org-window-habit-mode
      (org-window-habit-insert-consistency-graphs)
    (apply orig args)))

(advice-add 'org-habit-insert-consistency-graphs
            :around 'org-window-habit-insert-consistency-graphs-advice)

(defun org-window-habit-get-urgency-advice (orig &rest args)
  (if org-window-habit-mode
      org-default-priority              ;TODO fix this
    (apply orig args)))

(advice-add 'org-habit-get-urgency
            :around 'org-window-habit-get-urgency-advice)

(defun org-window-habit-auto-repeat (done-word)
  (save-excursion
    (let ((scheduled (org-entry-get (point) "SCHEDULED")))
      (when scheduled
	    (org-remove-timestamp-with-keyword org-scheduled-string)))))

(defun org-window-habit-auto-repeat-maybe-advice (orig &rest args)
  (apply orig args)
  ;; (if (and org-window-habit-mode (org-is-habit-p))
  ;;     (apply 'org-window-habit-auto-repeat args)
  ;;   (apply orig args))
  )

(advice-add 'org-auto-repeat-maybe
            :around 'org-window-habit-auto-repeat-maybe-advice)

(advice-add 'org-element--property
            :around 'org-window-habit-scheduled-deadline-hackery)

(defun org-window-habit-scheduled-deadline-hackery (orig property node &rest args)
  (let ((actual-value (apply orig property node args))
        (is-habit (string= "habit" (funcall orig :STYLE node))))
    actual-value
    ;; (message "prop: %s, is-habit: %s" property is-habit)
    ;; (if (and org-window-habit-mode (string= "habit" (funcall orig :STYLE node)))
    ;;     (cond
    ;;      ((eq property :deadline) nil)
    ;;      ((eq property :scheduled) (or actual-value (apply orig :deadline node args)))
    ;;      (t actual-value))
    ;;   actual-value)
    ))

(defun org-window-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t)
	(buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p))
          (invisible-prop (get-text-property (point) 'invisible)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			              (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-window-habit-make-graph-string
          (org-window-habit-build-graph habit))))
      ;; Inherit invisible state of hidden entries.
      ;; (when invisible-prop
      ;;   (put-text-property
      ;;    (- (point) org-habit-graph-column) (point)
      ;;    'invisible invisible-prop))))
	  (forward-line))))))

(provide 'org-window-habit)
