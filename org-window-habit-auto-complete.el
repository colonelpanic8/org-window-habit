;;; org-window-habit-auto-complete.el --- Automatic completion support -*- lexical-binding: t; -*-

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

;; Timer-driven automatic completion for org-window-habit entries.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'subr-x)
(require 'org-window-habit-config)
(require 'org-window-habit-core)
(require 'org-window-habit-instance)
(require 'org-window-habit-logbook)

;; Forward declarations
(declare-function org-window-habit-entry-p "org-window-habit")
(declare-function org-window-habit-property "org-window-habit")
(declare-function org-window-habit-auto-repeat "org-window-habit-advice")


;;; Customization

(defcustom org-window-habit-auto-complete-interval (* 30 60)
  "Base delay in seconds between automatic completion scans."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-auto-complete-jitter (* 5 60)
  "Maximum random seconds added to each automatic completion scan delay."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-auto-complete-prefetch-function nil
  "Function called before automatic completion scans.
This is intended for conservative synchronization, such as a git
fast-forward pull.  If the function signals an error, the scan is aborted."
  :group 'org-window-habit
  :type '(choice (const :tag "None" nil) function))

(defcustom org-window-habit-auto-complete-after-save-function nil
  "Function called after automatic completion saves a changed Org buffer."
  :group 'org-window-habit
  :type '(choice (const :tag "None" nil) function))


;;; Timer state

(defvar org-window-habit-auto-complete--timer nil
  "Current automatic completion timer.")

(defvar org-window-habit-auto-complete-mode)


;;; Config access

(defun org-window-habit-auto-complete--function-for-time (&optional time)
  "Return the configured automatic completion predicate for TIME.
The current Org entry must be an org-window-habit entry.  The predicate comes
from the active config's :auto-complete-fn value."
  (let ((config-str (org-entry-get nil (org-window-habit-property "CONFIG") t)))
    (when config-str
      (let* ((configs (org-window-habit-parse-config config-str))
             (config (or (org-window-habit-get-config-for-time
                          configs (or time (current-time)))
                         (car configs)))
             (fn (plist-get config :auto-complete-fn)))
        (cond
         ((and (consp fn) (eq (car fn) 'function)) (cadr fn))
         ((or (functionp fn) (symbolp fn)) fn))))))


;;; Completion detection

(defun org-window-habit-auto-complete-current-assessment-completion-p
    (&optional now)
  "Return non-nil when the current entry has a completion in NOW's interval.
NOW defaults to `current-time'."
  (setq now (or now (current-time)))
  (let* ((habit (org-window-habit-create-instance-from-heading-at-point))
         (spec (car (oref habit window-specs)))
         (window (org-window-habit-get-assessment-window spec now)))
    (cl-loop for done-time across (oref habit done-times)
             thereis
             (org-window-habit-time-falls-in-assessment-interval
              window done-time))))


;;; Completion scanning

(defun org-window-habit-auto-complete--revert-unmodified-agenda-buffers ()
  "Revert unmodified agenda file buffers after a prefetch operation."
  (dolist (file (org-agenda-files))
    (let ((buffer (get-file-buffer file)))
      (when (and buffer
                 (buffer-live-p buffer)
                 (not (buffer-modified-p buffer)))
        (with-current-buffer buffer
          (revert-buffer :ignore-auto :noconfirm))))))

(defun org-window-habit-auto-complete--modified-agenda-buffers ()
  "Return modified buffers visiting `org-agenda-files'."
  (cl-loop for file in (org-agenda-files)
           for buffer = (get-file-buffer file)
           when (and buffer
                     (buffer-live-p buffer)
                     (buffer-modified-p buffer))
           collect buffer))

(defun org-window-habit-auto-complete--ensure-agenda-buffers-unmodified ()
  "Signal an error if any agenda file buffer has unsaved changes."
  (let ((buffers (org-window-habit-auto-complete--modified-agenda-buffers)))
    (when buffers
      (user-error
       "Refusing auto-completion with modified agenda buffers: %s"
       (mapconcat #'buffer-name buffers ", ")))))

(defun org-window-habit-auto-complete--done-state-p ()
  "Return non-nil if the current Org heading is already in a done state."
  (member (org-entry-get nil "TODO") org-done-keywords))

(defun org-window-habit-auto-complete--complete-at-point (now)
  "Complete the Org entry at point using NOW as the completion time."
  (let ((from-state (or (org-entry-get nil "TODO") "TODO"))
        (to-state (car org-done-keywords)))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (let ((org-log-done nil))
        (org-todo 'done)))
    (unless (org-window-habit-auto-complete-current-assessment-completion-p
             now)
      (org-window-habit-auto-complete--insert-state-log
       to-state from-state now)
      (org-window-habit-fix-logbook-order)
      (when (fboundp 'org-window-habit-auto-repeat)
        (org-window-habit-auto-repeat)))))

(defun org-window-habit-auto-complete--logbook-insertion-point ()
  "Return a point where a new LOGBOOK entry should be inserted.
Creates a LOGBOOK drawer for the current entry if needed."
  (save-excursion
    (org-back-to-heading t)
    (let ((entry-end (save-excursion (org-entry-end-position))))
      (if (re-search-forward org-logbook-drawer-re entry-end t)
          (let ((drawer-end (save-excursion
                              (re-search-forward "^[ \t]*:END:[ \t]*$"
                                                 entry-end t))))
            (unless drawer-end
              (error "Malformed LOGBOOK drawer"))
            (goto-char drawer-end)
            (line-beginning-position))
        (org-end-of-meta-data t)
        (insert ":LOGBOOK:\n:END:\n")
        (forward-line -1)
        (point)))))

(defun org-window-habit-auto-complete--insert-state-log
    (to-state from-state time)
  "Insert a state-change log from FROM-STATE to TO-STATE at TIME."
  (save-excursion
    (goto-char (org-window-habit-auto-complete--logbook-insertion-point))
    (insert
     (format "- State \"%s\"       from \"%s\"       %s\n"
             to-state
             from-state
             (format-time-string (org-time-stamp-format nil t) time)))))

(defun org-window-habit-auto-complete--maybe-complete-at-point (now)
  "Maybe automatically complete the org-window-habit entry at point for NOW."
  (when (and (org-window-habit-entry-p)
             (not (org-window-habit-auto-complete--done-state-p))
             (not (org-window-habit-auto-complete-current-assessment-completion-p
                   now)))
    (let* ((habit (org-window-habit-create-instance-from-heading-at-point))
           (marker (point-marker))
           (fn (org-window-habit-auto-complete--function-for-time now)))
      (when (and fn (funcall fn marker habit now))
        ;; Re-check immediately before mutating the buffer.  This is the local
        ;; idempotency guard if the predicate had side effects or took time.
        (unless (org-window-habit-auto-complete-current-assessment-completion-p
                 now)
          (org-window-habit-auto-complete--complete-at-point now)
          t)))))

;;;###autoload
(defun org-window-habit-auto-complete-run (&optional now)
  "Scan agenda files and automatically complete eligible habits.
NOW defaults to `current-time'.  Returns the number of completed entries."
  (interactive)
  (setq now (or now (current-time)))
  (org-window-habit-auto-complete--ensure-agenda-buffers-unmodified)
  (when org-window-habit-auto-complete-prefetch-function
    (funcall org-window-habit-auto-complete-prefetch-function)
    (org-window-habit-auto-complete--revert-unmodified-agenda-buffers))
  (let ((completed-count 0))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (let ((file-completed-count 0))
            (org-with-wide-buffer
             (org-map-entries
              (lambda ()
                (when (org-window-habit-auto-complete--maybe-complete-at-point
                       now)
                  (cl-incf completed-count)
                  (cl-incf file-completed-count)))))
            (when (> file-completed-count 0)
              (save-buffer)
              (when org-window-habit-auto-complete-after-save-function
                (funcall org-window-habit-auto-complete-after-save-function)))))))
    (when (called-interactively-p 'interactive)
      (message "Automatically completed %d org-window-habit entries"
               completed-count))
    completed-count))


;;; Timer mode

(defun org-window-habit-auto-complete--next-delay ()
  "Return the delay before the next automatic completion scan."
  (+ org-window-habit-auto-complete-interval
     (if (and org-window-habit-auto-complete-jitter
              (> org-window-habit-auto-complete-jitter 0))
         (random org-window-habit-auto-complete-jitter)
       0)))

(defun org-window-habit-auto-complete--timer-callback ()
  "Run automatic completion and schedule the next scan."
  (setq org-window-habit-auto-complete--timer nil)
  (unwind-protect
      (org-window-habit-auto-complete-run)
    (when org-window-habit-auto-complete-mode
      (org-window-habit-auto-complete--schedule-next-run))))

(defun org-window-habit-auto-complete--schedule-next-run ()
  "Schedule the next automatic completion scan."
  (when org-window-habit-auto-complete--timer
    (cancel-timer org-window-habit-auto-complete--timer))
  (setq org-window-habit-auto-complete--timer
        (run-at-time
         (org-window-habit-auto-complete--next-delay)
         nil
         #'org-window-habit-auto-complete--timer-callback)))

;;;###autoload
(define-minor-mode org-window-habit-auto-complete-mode
  "Global mode for timer-driven automatic org-window-habit completion."
  :lighter nil
  :global t
  :group 'org-window-habit
  (if org-window-habit-auto-complete-mode
      (org-window-habit-auto-complete--schedule-next-run)
    (when org-window-habit-auto-complete--timer
      (cancel-timer org-window-habit-auto-complete--timer)
      (setq org-window-habit-auto-complete--timer nil))))


;;; Git prefetch helper

(defun org-window-habit-auto-complete--git-root ()
  "Return the current git repository root, or signal an error."
  (or (locate-dominating-file default-directory ".git")
      (user-error "Not inside a git repository")))

(defun org-window-habit-auto-complete--agenda-git-root ()
  "Return the single git root containing all existing agenda files.
If agenda files are not all in one git repository, signal an error."
  (let* ((roots
          (delete-dups
           (cl-loop for file in (org-agenda-files)
                    for directory = (file-name-directory
                                     (expand-file-name file))
                    for root = (and (file-exists-p file)
                                    (let ((default-directory directory))
                                      (locate-dominating-file directory ".git")))
                    when root
                    collect (file-truename root)))))
    (cond
     ((null roots) nil)
     ((null (cdr roots)) (car roots))
     (t
      (user-error
       "Refusing git prefetch across multiple agenda repositories: %s"
       (mapconcat #'identity roots ", "))))))

(defun org-window-habit-auto-complete--git-call (&rest args)
  "Run git with ARGS, returning the command output or signaling an error."
  (unless (executable-find "git")
    (user-error "Cannot prefetch because git is not available"))
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil args)))
      (unless (zerop status)
        (user-error "Git command failed: git %s\n%s"
                    (mapconcat #'identity args " ")
                    (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun org-window-habit-auto-complete--git-ok-p (&rest args)
  "Return non-nil if git exits successfully with ARGS."
  (unless (executable-find "git")
    (user-error "Cannot prefetch because git is not available"))
  (zerop (apply #'call-process "git" nil nil nil args)))

(defun org-window-habit-auto-complete--git-clean-p ()
  "Return non-nil if the current git worktree is completely clean."
  (and
   (org-window-habit-auto-complete--git-ok-p
    "diff" "--quiet" "--ignore-submodules" "--")
   (org-window-habit-auto-complete--git-ok-p
    "diff" "--cached" "--quiet" "--ignore-submodules" "--")
   (string-empty-p
    (org-window-habit-auto-complete--git-call
     "ls-files" "--others" "--exclude-standard"))))

;;;###autoload
(defun org-window-habit-auto-complete-prefetch-git-ff-only (&optional directory)
  "Fetch and fast-forward the current git repository if it is safe.
The worktree must be completely clean, including no untracked files.  The
current branch must have an upstream, and the update must be a fast-forward.
If DIRECTORY is nil, use the single git repository containing the current
`org-agenda-files', falling back to `default-directory'."
  (interactive)
  (let ((default-directory
         (file-name-as-directory
          (or directory
              (org-window-habit-auto-complete--agenda-git-root)
              (org-window-habit-auto-complete--git-root)))))
    (unless (org-window-habit-auto-complete--git-clean-p)
      (user-error "Refusing to prefetch with a dirty git worktree"))
    (let ((upstream (org-window-habit-auto-complete--git-call
                     "rev-parse" "--abbrev-ref" "--symbolic-full-name"
                     "@{upstream}")))
      (org-window-habit-auto-complete--git-call "fetch" "--prune")
      (cond
       ((org-window-habit-auto-complete--git-ok-p
         "merge-base" "--is-ancestor" "HEAD" upstream)
        (org-window-habit-auto-complete--git-call
         "merge" "--ff-only" upstream))
       ((org-window-habit-auto-complete--git-ok-p
         "merge-base" "--is-ancestor" upstream "HEAD")
        nil)
       (t
        (user-error "Refusing to prefetch because the branch diverged"))))))

(provide 'org-window-habit-auto-complete)
;;; org-window-habit-auto-complete.el ends here
