;;; org-timeline.el --- Add graphical view of agenda to agenda buffer -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.3.0
;; Created: 16th April 2017
;; Package-requires: ((dash "2.13.0") (emacs "26.1"))
;; Keywords: calendar
;; URL: https://github.com/Fuco1/org-timeline/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add graphical view of agenda to agenda buffer.

;; This package adds a graphical view of the agenda after the last
;; agenda line.  By default the display starts at 5 AM today and
;; goes up to 4 AM next day (this covers 24 hours).

;; Scheduled tasks or tasks with time ranges are rendered in the
;; display with `org-timeline-block' face.  Clocked entires are
;; displayed in `org-timeline-clocked' face.  The background of
;; timeslots which are in the past is highlighted with
;; `org-timeline-elapsed' face.

;; You can use custom color for a task by adding the property
;; `TIMELINE_FACE' with either a string which is a color name or a
;; list which specifies the face properties or a symbol which is
;; taken to be a face name.

;;; Code:

(require 'dash)

(require 'org-agenda)

(defgroup org-timeline ()
  "Graphical view of agenda in agenda buffer."
  :group 'org
  :prefix "org-timeline-")

(defgroup org-timeline-faces ()
  "Faces for org-timeline."
  :group 'org-timeline)

(defcustom org-timeline-prepend nil
  "Option to prepend the timeline to the agenda."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-show-clocked t
  "Option to show or hide clocked items."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-dedicated-clocked-line t
  "Option to show clocked items in a dedicated line with 'group-name' '$'."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-overlap-in-new-line nil
  "Option to create new lines for blocks that would otherwise overlap."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-emphasize-next-block nil
  "Option to apply the face `org-timeline-next-block' to the next block happening today."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-emphasize-priority 'c
  "Option to apply the face `org-timeline-priority-block' to blocks with priority greater than or equal to the target value. Allows the symbols a, b, and c, as aliases for their corresponding org priorities."
  :group 'org-timeline)
(defvar org-timeline-priority-matches '((a . 2000) (b . 1000) (c . 0)))

(defcustom org-timeline-tag-faces nil
  "Optional plist to set faces based on certain tags. Overrides the default and priority-based formatting.
Tags are read in reverse order (tags earlier in the element's tag list are applied later, overriding tags later in the list).
Set a tag to a function taking propertized text and a task object and returning propertized text to directly manipulate the block (be careful not to change properties which might interfere with the timeline, such as length!)."
  :group 'org-timeline)

(defcustom org-timeline-show-text-in-blocks nil
  "Option to show the text of the event in the block.

If the item has a property `TIMELINE_TEXT', use this as a title.
Otherwise, the title will be the item's headline, stripped of its todo state."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-beginning-of-day-hour 5
  "When the timeline begins.

Due to the way 'org-agenda' works, if you set this to any other value than 0
\(e.g. 5), then events that happen after midnight will not appear (even though
the timeline shows the slots).
If you view the agenda in week mode, those events will not appear in any of
the week's day.

The workaround for this in day view is to use `org-timeline-keep-elapsed' that
will make the timeline show you a 24h cycle. See this variable's documentation
for more information."
  :type 'integer
  :group 'org-timeline)

(defcustom org-timeline-keep-elapsed -1
  "In day view, for today, keep only this number of fully elapsed hours.

For negative values, do not hide elapsed hours.

This can be used to see a rolling 24h cycle in the timeline.
In order to do that, set `org-timeline-beginning-of-day-hour' to 0, and set
`org-timeline-keep-elapsed' to any positive number.
Set `org-agenda-span' to 2, and open the day agenda view for today.
You will see a rolling 24h cycle, starting `org-timeline-keep-elapsed' hours ago."
  :type 'integer
  :group 'org-timeline)

(defcustom org-timeline-default-duration 10
  "The duration used to display timeline blocks that don't already have a duration. Set to nil to hide these blocks. Should otherwise be greater than org-timeline-minimum-duration"
  :type 'integer
  :group 'org-timeline)

(defconst org-timeline-minimum-duration 10 "The minimum duration of a timeline block. Used to upscale ones that are too small.")

(defcustom org-timeline-insert-before-text "\u25B6"
  "String inserted before the block's text.

It makes consecutive blocks distinct.

The default value '\u25B6' is a right-facing triangle ▶."
  :type 'string
  :group 'org-timeline)

(defcustom org-timeline-prefix-length 3
  "The length of the prefix at the left of a timeline.
Note that this should be at least 3 to avoid truncating
day-of-week names.")
(defun org-timeline--get-repeating-string (n rep)
  (apply #'concat (cl-loop for i below n collect rep)))
(defun org-timeline--get-empty-string-of-length (n)
  (org-timeline--get-repeating-string n " "))
(defun org-timeline--get-empty-prefix ()
  "Returns the string for a empty prefix, based on `org-timeline-prefix-length'"
  (org-timeline--get-empty-string-of-length org-timeline-prefix-length))
(defun org-timeline--get-clock-prefix ()
  "Returns the string for a empty prefix, based on `org-timeline-prefix-length'"
  (org-timeline--get-repeating-string org-timeline-prefix-length "$"))


(defcustom org-timeline-cursor-sensor t
  "Option to turn on cursor-sensor-mode in org-agenda detect when the text cursor enters / leaves a block, and run `org-timeline-cursor-entered-functions' or `org-timeline-cursor-left-functions' as appropriate."
  :type 'boolean
  :group 'org-timeline)

(setq-default org-timeline-cursor-based-update-just-occurred nil) ;; Ensure that we don't constantly update the info line while the cursor is over a block

(defcustom org-timeline-cursor-entered-functions (list (lambda (w pt)
                                                      (unless org-timeline-cursor-based-update-just-occurred
                                                        (save-mark-and-excursion
                                                          (org-timeline--draw-new-info w (get-text-property (point) 'task-info))
                                                          (setq org-timeline-cursor-based-update-just-occurred t)))))
  "Functions to run when the cursor enters a block. Each function should take 2 arguments, the window and the point position"
  :group 'org-timeline)

(defcustom org-timeline-cursor-left-functions (list (lambda (_ _) (setq org-timeline-cursor-based-update-just-occurred nil)))
  "Functions to run when the cursor leaves a block. Each function should take 2 arguments, the window and the point position"
  :group 'org-timeline)

(defvar org-timeline-first-line-in-agenda-buffer 0
  "Line number of the first line of the timeline in the agenda buffer.")

(defvar org-timeline-height 0
  "Final height of the timeline.")

(defvar org-timeline-current-info nil
  "Current displayed info. Used to fix flickering of info.")

(defvar org-timeline-slotline (concat (mapconcat 'not (number-sequence 0 24) "|     ") "|")
  "The undecorated slotline string.")

(defvar org-timeline-next-task-today nil
  "The next task happening today.")

(cl-defstruct org-timeline-task
  id
  beg  ; in minutes
  end  ; in minutes
  offset-beg ; in points
  offset-end ; in points
  info ; copy of the agenda buffer's line
  line-in-agenda-buffer
  face
  day  ; absolute, see `calendar-absolute-from-gregorian'
  type ; "scheduled", "clocked" ...
  text
  group-name
  do-not-overlap-p ; make sure this block doesn't overlap with any other
  org-marker
  org-hd-marker
  tags
  )


(defface org-timeline-block
  '((t (:inherit secondary-selection)))
  "Face used for printing blocks with time range information.

These are blocks that are scheduled for specific time range or
have an active timestamp with a range."
  :group 'org-timeline-faces)

(defface org-timeline-elapsed
  '((t (:inherit region)))
  "Face used for highlighting elapsed portion of the day."
  :group 'org-timeline-faces)

(defface org-timeline-clocked
  '((t (:inherit highlight)))
  "Face used for printing clocked blocks.

Clocked blocks appear in the agenda when `org-agenda-log-mode' is
activated."
  :group 'org-timeline-faces)

(defface org-timeline-clock-unelapsed
  '((t (:background "#404040")))
  "Face used for the unelapsed portions of clock lines"
  :group 'org-timeline-faces)

(defface org-timeline-clock-elapsed
  '((t (:inherit region :background "#3b3f49")))
  "Face used for highlighting elapsed portion of clock lines."
  :group 'org-timeline-faces)

(defface org-timeline-overlap
  '((t (:background "dark red")))
  "Face used for printing overlapping blocks."
  :group 'org-timeline-faces)

(defface org-timeline-next-block
  '((default :weight bold)
    (((background dark)) :background "#050" :foreground "#0ff")
    (((background light)) :background "#9f9" :foreground "#044"))
   "Face used for printing the next block happening today.

Used when `org-timeline-emphasize-next-block' is non-nil."
   :group 'org-timeline-faces)

(defface org-timeline-priority-block
  '((t (:background "#d48")))
  "Face used for printing blocks with high priority which aren't next.

Used when `org-timeline-emphasize-priority' is non-nil."
  :group 'org-timeline-faces)

(defface org-timeline-foreground
  '((default :weight bold)
    (((background light)) :foreground "black")
    (((background dark)) :foreground "white"))
  "Face for the foreground of a block. Added after backgrounds are chosen."
  :group 'org-timeline-faces)


(defmacro org-timeline-with-each-line (&rest body)
  "Execute BODY on each line in buffer."
  (declare (indent 0)
           (debug (body)))
  `(save-excursion
     (goto-char (point-min))
     ,@body
     (while (= (forward-line) 0)
       ,@body)))

(defun org-timeline--get-face (type)
  "Get the face with which to draw the current block, according to TYPE."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_FACE" t)
      (let ((read-face (car (read-from-string it))))
        (if (stringp read-face)
            (list :background read-face)
          read-face))
    (if (string= type "clock")
        (list 'org-timeline-clocked 'org-timeline-block)
      (list 'org-timeline-block))))

(defun org-timeline--get-block-text ()
  "Get the text to print inside the current block."
  (let ((item-marker (org-get-at-bol 'org-marker)))
    (--if-let (org-entry-get item-marker "TIMELINE_TEXT" t)
        it
       (with-current-buffer (marker-buffer item-marker)
        (save-excursion
          (goto-char item-marker)
          (outline-previous-heading)
          (let* ((heading-text (org-element-property :raw-value (org-element-context)))
                 (heading-text (replace-regexp-in-string "^\\[\\([0-9]+\\{1,3\\}%\\|[0-9]+/[0-9]+\\)\\] " "" heading-text)))
            heading-text))
        ;; (org-last (org-get-outline-path))
        ))))

(defun org-timeline--get-group-name (type)
  "Get the current block's 'group-name' according to TYPE.

The first three chars will be printed at the beginning of the block's line."
  (if (and (string= type "clock") org-timeline-dedicated-clocked-line)
      (org-timeline--get-clock-prefix)
    (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_GROUP" t)
        (if (< (length it) org-timeline-prefix-length)
            (concat (substring (org-timeline--get-empty-prefix) 0 (- org-timeline-prefix-length (length it))) it)
          (substring it 0 org-timeline-prefix-length))
      (org-timeline--get-empty-prefix))))

(defun org-timeline--get-do-not-overlap (type)
  "Whether the current block is allowed to  overlap in the timeline according to TYPE."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_DO_NOT_OVERLAP" t)
      it
    (if (and ;; (not (string= type "clock"))
            org-timeline-overlap-in-new-line)
        t
      nil)))

(defun org-timeline--overlapping-at-point (task)
  "List of points where an already drawn blocks would overlap with TASK."
  (save-excursion
    (let (overlap-points)
      (goto-char (+ (line-beginning-position)
                    (org-timeline-task-offset-beg task)))
      (while (and (<= (point) (+ (line-beginning-position) (org-timeline-task-offset-end task)))
                 (< (point) (point-max)))
        (when (get-text-property (point) 'org-timeline-occupied)
          (push (point) overlap-points))
        (forward-char))
      overlap-points)))

(defun org-timeline--new-overlap-line-required-at-point-p (task)
  "Whether a new overlap line needs to be created to insert TASK."
  (let* ((overlapping (org-timeline--overlapping-at-point task))
         (overlapping-blocks-that-do-not-overlap
          (delq nil (mapcar (lambda (point) (get-text-property point 'org-timeline-do-not-overlap)) overlapping))))
    (and (not (eq overlapping nil))
         (or (org-timeline-task-do-not-overlap-p task)
             (not (eq overlapping-blocks-that-do-not-overlap nil))))))

(defun org-timeline--add-elapsed-face (string &optional group-name day)
  "Add `org-timeline-elapsed' to STRING's elapsed portion.

Return new copy of STRING."
  (let* ((string-copy (copy-sequence string))
         (start-offset (* org-timeline-beginning-of-day-hour 60))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M"))))
         (current-offset (/ (- current-time start-offset) 10)))
    (when (< 0 current-offset)
      (put-text-property 0 (+ 1 current-offset) 'face (list 'org-timeline-elapsed) string-copy))
    (when (and group-name (string-match-p (regexp-quote (org-timeline--get-clock-prefix)) group-name))
      (put-text-property 0 (length string-copy) 'face (list 'org-timeline-clock-unelapsed) string-copy)
      (put-text-property 0 (+ 1 current-offset) 'face (list 'org-timeline-clock-elapsed) string-copy))
    string-copy))

(defun org-timeline--kill-info ()
  "Kill the info line."
  (save-excursion
    (goto-line org-timeline-first-line-in-agenda-buffer)
    (while (and (not (get-text-property (point) 'org-timeline-info-line))
                (eq (forward-line) 0)))
    (unless (eq (point) (point-max)) ; info line not found
      (let ((inhibit-read-only t))
        (kill-whole-line)))))

(defun org-timeline--decorate-info (info)
  "Make INFO string clickable."
  (let ((info-keymap (make-sparse-keymap)))
    (define-key info-keymap [mouse-1] 'org-agenda-goto)
    (define-key info-keymap [mouse-2] 'org-find-file-at-mouse)
    (propertize info 'keymap info-keymap
                     'help-echo "mouse-1 jump to org file"
                     'org-timeline-info-line t)))

(defun org-timeline--draw-new-info (win info)
  "Displays INFO about a hovered block.

WIN is the agenda buffer's window."
  (unless (eq info org-timeline-current-info) ; prevents flickering
    (setq org-timeline-current-info info)
    (save-window-excursion
      (save-excursion
        (select-window win) ; because one can hover blocks without being in the agenda window.
        (org-timeline--kill-info)
        (goto-line org-timeline-first-line-in-agenda-buffer)
        (forward-line (- org-timeline-height 2))
        (let ((inhibit-read-only t))
          (insert (org-timeline--decorate-info info) "\n"))))))

(defun org-timeline--move-to-task-in-agenda-buffer ()
  "Move to a block's correponding task in the agenda buffer."
  (interactive)
   (let ((line (get-text-property (point) 'org-timeline-task-line)))
     (when org-timeline-prepend
       (setq line (+ line org-timeline-height -1)))
     (goto-line line)
     (search-forward (get-text-property (point) 'time)))) ; makes point more visible to user.

(defun org-timeline--switch-to-task-in-file (task-info)
  (interactive (list (get-text-property (point) 'task-info)))
  (let ((target-marker (get-text-property 0 'org-marker task-info)))
    (let ((target-buffer (marker-buffer target-marker)))
      (when target-buffer
        (switch-to-buffer target-buffer)
        (goto-char (marker-position target-marker))))))

(defun org-timeline--goto-task-in-file (task-info)
  (interactive (list (get-text-property (point) 'task-info)))
  (let ((target-marker (get-text-property 0 'org-marker task-info)))
    (let ((target-buffer (marker-buffer target-marker)))
      (when target-buffer
        (pop-to-buffer target-buffer)
        (goto-char (marker-position target-marker))))))

(defun org-timeline--cursor-sensor-functions (win pt event)
  (interactive)
  (when org-timeline-cursor-sensor
    (cond
     ((equal event 'entered) (mapcar (lambda (f) (funcall-interactively f win pt)) org-timeline-cursor-entered-functions))
     ((equal event 'left) (mapcar (lambda (f) (funcall-interactively f win pt)) org-timeline-cursor-left-functions)))))

(defun org-timeline--cursor-over-block ()
  (when (member 'org-timeline-block (face-at-point nil t)) t))

(defun org-timeline--list-tasks ()
  "Build the list of tasks to display."
  (let* ((tasks nil)
         (max-day 0)
         (id 0)
         (start-offset (* org-timeline-beginning-of-day-hour 60))
         (end-offset (+ start-offset (* 24 60)))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M")))))
    (org-timeline-with-each-line
      (-when-let (day (org-get-at-bol 'day))
        (setq max-day (max max-day day))))
    (org-timeline-with-each-line
      (-when-let* ((day (org-get-at-bol 'day))
                   (time-of-day (or (org-get-at-bol 'time-of-day) 'no-time))
                   (marker (org-get-at-bol 'org-marker))
                   ;; Figure out how I can remove this to allow tracking Diary entries
                   (hd-marker (org-get-at-bol 'org-hd-marker))
                   (type (org-get-at-bol 'type))
                   (dotimes (-if-let (dotime (org-get-at-bol 'dotime)) dotime ""))
                   (dt (if dotimes dotimes ""))
                   (day-append (-if-let* ((days (org-get-at-bol 'org-day-cnt))
                                         (stringdt (stringp dt))
                                         (norepeat (not (string-match-p (regexp-quote "+") dt)))
                                         (daycount (> days 1))
                                         (end-time (-when-let* ((isstring (stringp dt))
                                                                (end-idx (string-match-p "<\\(.*:.*\\)$" dt))
                                                                (end-str (concat (substring dt end-idx) ">")))
                                                     (org-get-time-of-day end-str)))
                                         (day-duration (-if-let* ((tod (if (numberp time-of-day) time-of-day 0))
                                                                  (upper (- (truncate (/ end-time 100)) (truncate (/ tod 100))))
                                                                  (lower (- (% end-time 100) (% tod 100)))
                                                                  (overlap (truncate (/ lower 60)))
                                                                  (lower (- lower (* 60 overlap))))
                                                           (+ (* 60 upper) (* 60 overlap) lower))))
                                  (* (max 0 (1- days)) (* 60 24))
                                0))
                   (duration (-if-let (duration (org-get-at-bol 'duration))
                                 (if (string= type "clock") (+ duration day-append) duration)
                               (if (and (string= type "clock")
                                       (find-if (lambda (ov) (equal (overlay-get ov 'face) 'org-agenda-clocking))
                                                (overlays-at (point) (point))))
                                   'current-clock
                                 (-if-let* ((gate nil)
                                            (days (org-get-at-bol 'org-day-cnt))
                                            (stringdt (stringp dt))
                                            (norepeat (not (string-match-p (regexp-quote "+") dt)))
                                            (daycount (> days 1))
                                            (end-time (-when-let* ((end-idx (string-match-p "<\\(.*:.*\\)$" dt))
                                                                   (end-str (concat (substring dt end-idx) ">")))
                                                        (org-get-time-of-day end-str)))
                                            (day-duration (-if-let* ((tod (if (numberp time-of-day) time-of-day 0))
                                                                     (upper (- (truncate (/ end-time 100)) (truncate (/ tod 100))))
                                                                     (lower (- (% end-time 100) (% tod 100)))
                                                                     (overlap (truncate (/ lower 60)))
                                                                     (lower (- lower (* 60 overlap))))
                                                              (+ (* 60 upper) (* 60 overlap) lower))))
                                     (progn  (print (format "inside %s %s" day-append day-duration)) (+ day-append day-duration))
                                   org-timeline-default-duration)))))
        (when (eq time-of-day 'no-time)
          (setq time-of-day 0000)
          (setq duration org-timeline-default-duration)
          ;; (message "day %s time %s duration %s marker %s" day time-of-day duration marker)
          )
        (when (eq duration 'current-clock) (setq duration nil))
        (when (member type (list "past-scheduled" "scheduled" "clock" "timestamp"))
          (when (numberp duration)
            (when (and (< duration 0) (>= duration -1440))
              (cl-incf duration 1440)))
          (let ((tags (org-get-at-bol 'tags)))
           (let* ((hour (/ time-of-day 100))
                  (minute (mod time-of-day 100))
                  (still-drawing t)
                  (init-beg (+ (* hour 60) minute))
                  (init-end (if duration
                                (round (+ init-beg (max duration org-timeline-minimum-duration)))
                              (max (+ current-time (* 60 24 (- (time-to-days (current-time)) day)))
                                   (round (+ init-beg org-timeline-default-duration)))))
                  (beg (max init-beg start-offset))
                  (duration (- init-end beg))
                  (end (min init-end end-offset)))
             (while (and (>= end start-offset)
                         (<= beg end-offset)
                         (or org-timeline-show-clocked
                             (not (string= type "clock")))
                         (<= day max-day)
                         still-drawing)
               (when (eq end (* 24 60)) (cl-incf end -1)) ; FIXME fixes a bug that shouldn't happen (crash when events end at midnight).
               ;; (message "hour %s minute %s beg %s duration %s end %s text %s" hour minute beg duration end (org-timeline--get-block-text))
               (let* ((offset-prefix (+ 2 org-timeline-prefix-length))
                      (timeline-group (org-timeline--get-group-name type))
                      (new-task (make-org-timeline-task
                                 :id id
                                 :beg beg
                                 :end end
                                 :offset-beg (+ offset-prefix (- (/ beg 10) (* 6 org-timeline-beginning-of-day-hour)))
                                 :offset-end (+ offset-prefix (- (/ end 10) (* 6 org-timeline-beginning-of-day-hour)))
                                 :info (buffer-substring (line-beginning-position) (line-end-position))
                                 :line-in-agenda-buffer (line-number-at-pos)
                                 :face (org-timeline--get-face type)
                                 :day day
                                 :type type
                                 :text (org-timeline--get-block-text)
                                 :group-name timeline-group
                                 :do-not-overlap-p (org-timeline--get-do-not-overlap type)
                                 :org-marker marker
                                 :org-hd-marker hd-marker
                                 :tags tags
                                 )))
                 ;; TODO: Figure out how to ensure the clock tasks are the first non-main group,
                 ;; if they exist
                 (push new-task tasks))
               (if (> init-end end-offset)
                   (progn
                     (cl-incf day)
                     (cl-decf duration (- end beg))
                     (setq init-beg 0)
                     (setq beg (max init-beg start-offset))
                     (setq init-end (+ beg duration))
                     (setq end (min init-end end-offset)))
                 (setq still-drawing nil))
               (cl-incf id)))))))
    ;; Move clock tasks to be displayed lowest
    (let ((is-clock (lambda (task)
                      (equal (org-timeline-task-group-name task)
                             (org-timeline--get-clock-prefix)))))
      (setq tasks (cl-concatenate 'list (cl-remove-if-not is-clock tasks) (cl-remove-if is-clock tasks))))
    ;; Make sure the oldest day has a representative at the end
    ;; (which becomes the beginning at the end of the method)
    ;; Note that since we take the last sample (i.e. the first sample in the output)
    ;; through using < instead of <=, this shouldn't interfere with ordering between groups
    (let* ((earliest-task-idx (cl-reduce (lambda (i j)
                                           (if (< (org-timeline-task-day (nth i tasks))
                                                   (org-timeline-task-day (nth j tasks)))
                                               i j))
                                         (cl-loop for i below (length tasks)
                                                  collect i)
                                         :initial-value 0))
           (earliest-task (nth earliest-task-idx tasks)))
      (setq tasks (cl-concatenate 'list
                                  (subseq tasks 0 earliest-task-idx)
                                  (subseq tasks (1+ earliest-task-idx))
                                  (list earliest-task))))
    ;; find the next task
    (setq org-timeline-next-task nil)
    (dolist (task tasks)
      (let* ((beg (org-timeline-task-beg task))
             (end (org-timeline-task-end task))
             (today (calendar-absolute-from-gregorian (calendar-current-date)))
             (is-today (eq today (org-timeline-task-day task)))
             (is-now (and (<= beg current-time)
                          (>= end current-time)))
             (is-after (> beg current-time))
             (is-closer-to-now (and is-after
                                    (or (eq org-timeline-next-task nil)
                                        (< beg (org-timeline-task-beg org-timeline-next-task)))))
             (is-clocked (string= "clock" (org-timeline-task-type task))))
        (when (and is-today (or is-now is-closer-to-now) (not is-clocked))
          (setq org-timeline-next-task task))))
    (when org-timeline-next-task
      (setq org-timeline-next-task-today org-timeline-next-task))
    ;; change high-priority task faces
    (when org-timeline-emphasize-priority
      (let ((threshold (if-let ((pval (alist-get org-timeline-emphasize-priority org-timeline-priority-matches)))
                           pval
                         org-timeline-emphasize-priority))
            (value-matches (mapcar (lambda (c) (cons (cdr c) (car c))) org-timeline-priority-matches)))
        (dolist (task tasks)
          (let ((curr-priority (when (string-match org-priority-regexp (org-timeline-task-info task))
                                 (org-get-priority (org-timeline-task-info task)))))
            (when (and curr-priority
                       (>= curr-priority threshold))
              (setf (org-timeline-task-face task)
                    (cl-remove-if-not #'identity
                                   (cl-concatenate 'list
                                    (list (if-let ((curr-priority-name (alist-get curr-priority value-matches)))
                                           (list :background
                                                 (face-attribute (org-get-priority-face (upcase (string-to-char (symbol-name curr-priority-name))))
                                                                 :foreground))
                                         nil)
                                       'org-timeline-priority-block)
                                    (org-timeline-task-face task)))))))))
    ;; change the next task's face
    (when (and org-timeline-emphasize-next-block
               org-timeline-next-task)
      (dolist (task tasks)
        (when (eq (org-timeline-task-id task) (org-timeline-task-id org-timeline-next-task))
          (setf (org-timeline-task-face task) (list 'org-timeline-next-block (org-timeline-task-face task))))))
    ;; change the foreground to be more readable
    (dolist (task tasks)
      (setf (org-timeline-task-face task) (cl-concatenate 'list (org-timeline-task-face task) '(org-timeline-foreground))))
    (dolist (task tasks)
      (when-let ((filter-func (lambda (x)
                                (let ((deprop x))
                                  (set-text-properties 0 (length deprop) nil deprop)
                                  (lax-plist-get org-timeline-tag-faces deprop))))
                 (tag-names (seq-filter filter-func
                                    (reverse (org-timeline-task-tags task))))
                 (tag-faces (mapcar filter-func tag-names)))
        (dolist (tag-face tag-faces)
          (setf (org-timeline-task-face task) (list tag-face (org-timeline-task-face task))))))
    ;; Sort tasks by day, then by start time within each day, with clock tasks last, then by priority
    (setq tasks (sort tasks (lambda (a b)
                              (let ((day-a (org-timeline-task-day a))
                                    (day-b (org-timeline-task-day b))
                                    (beg-a (org-timeline-task-beg a))
                                    (beg-b (org-timeline-task-beg b))
                                    (is-clock-a (equal (org-timeline-task-group-name a) (org-timeline--get-clock-prefix)))
                                    (is-clock-b (equal (org-timeline-task-group-name b) (org-timeline--get-clock-prefix)))
                                    (priority-a (when (string-match org-priority-regexp (org-timeline-task-info a))
                                                  (org-get-priority (org-timeline-task-info a))))
                                    (priority-b (when (string-match org-priority-regexp (org-timeline-task-info b))
                                                  (org-get-priority (org-timeline-task-info b)))))
                                (if (= day-a day-b)
                                    (if (and is-clock-a (not is-clock-b))
                                        nil  ; clock task goes after non-clock
                                      (if (and (not is-clock-a) is-clock-b)
                                          t    ; non-clock task goes before clock
                                        (if (= beg-a beg-b)
                                            ;; Same time: sort by priority (higher priority = higher number = earlier)
                                            (> (or priority-a 0) (or priority-b 0))
                                          (< beg-a beg-b))))  ; different times, sort by time
                                  (< day-a day-b))))))
    tasks))

(defun org-timeline--goto-block-position (task)
  "Go to TASK's block's line and position cursor in line...

Return t if this task will overlap another one when inserted."
  (let* ((offset-beg (org-timeline-task-offset-beg task))
         (offset-end (org-timeline-task-offset-end task))
         (day (org-timeline-task-day task))
         (group-name (org-timeline-task-group-name task))
         (slotline (org-timeline--add-elapsed-face org-timeline-slotline))
         (do-not-overlap (org-timeline-task-do-not-overlap-p task))
         (is-today (= day (time-to-days (current-time))))
         (today-face '(:inherit secondary-selection :weight bold :underline t :overline t)))
    (goto-char 1)
    (while (and (not (eq (get-text-property (point) 'org-timeline-day) day))
                (not (eq (forward-line) 1))))
    (unless (eq (get-text-property (point) 'org-timeline-day) day)
      (insert (concat "\n" ; creating the necessary lines, up to the current task's day
                      (mapconcat (lambda (line-day)
                                   (let* ((line-date (calendar-gregorian-from-absolute line-day))
                                          (is-today (equal line-date (calendar-current-date)))
                                          ;; found in https://github.com/deopurkar/org-timeline
                                          (day-name (calendar-day-name (mod line-day 7) t t))
                                          (day-insert-string (concat
                                                              day-name
                                                              (org-timeline--get-empty-string-of-length
                                                               (- org-timeline-prefix-length
                                                                  (length day-name))))))
                                     (propertize
                                      (concat
                                       (propertize day-insert-string
                                                   'face (if is-today today-face nil))
                                       " "
                                       (org-timeline--add-elapsed-face slotline nil line-day)
                                       " "
                                       (propertize (calendar-date-string line-date t t)
                                                   'face (if is-today today-face nil)))
                                      'org-timeline-day line-day
                                      'org-timeline-group-name (org-timeline--get-empty-prefix))))
                                 (if-let ((last-day (get-text-property (line-beginning-position) 'org-timeline-day)))
                                     (number-sequence (+ 1 last-day) day)
                                   (list day))
                                 "\n"))))
    ;; cursor is now at beginning of the task's day's first line
    (while (and (not (string= (get-text-property (point) 'org-timeline-group-name) group-name))
                (eq (get-text-property (point) 'org-timeline-day) day))
      (forward-line))
    (unless (string= (-if-let (group-here (get-text-property (point) 'org-timeline-group-name)) group-here (org-timeline--get-empty-prefix)) group-name)
      (when (not (eq (line-end-position) (point-max))) (forward-line -1))
      (goto-char (line-end-position))
      (insert "\n"
              (let ((group-insert-string (concat group-name
                                                 (org-timeline--get-empty-string-of-length
                                                  (- org-timeline-prefix-length (length group-name))))))
                (propertize (concat (propertize group-insert-string 'face (when is-today today-face)) " " (org-timeline--add-elapsed-face slotline group-name))
                            'org-timeline-day day
                            'org-timeline-group-name group-name))))
    ;; cursor is now at beginning of the task's group's first line
    (let ((new-overlap-line-required-flag (org-timeline--new-overlap-line-required-at-point-p task)))
      (while (and new-overlap-line-required-flag ;; (org-timeline--new-overlap-line-required-at-point-p task)
                  (eq (get-text-property (point) 'org-timeline-day) day)
                  (equal (get-text-property (point) 'org-timeline-group-name) group-name)
                  (not (eq (line-end-position) (point-max))))
        ;; (setq new-overlap-line-required-flag t)
        (forward-line)
        (setq new-overlap-line-required-flag (org-timeline--new-overlap-line-required-at-point-p task)))
      (let ((decorated-slotline (propertize (concat (org-timeline--get-empty-prefix) " " (org-timeline--add-elapsed-face slotline group-name))
                                            'org-timeline-day day
                                            'org-timeline-group-name group-name
                                            'org-timeline-occupied nil))
            (group-conflict (not (string= (-if-let (group-here (get-text-property (point) 'org-timeline-group-name)) group-here group-name) group-name))))
        (when (or
               (when group-conflict
                 (forward-line -1))
               new-overlap-line-required-flag)
          (end-of-line)
          (insert "\n" decorated-slotline))))
    ;; cursor is now placed on the right line, at the right position.
    (goto-char (+ (line-beginning-position) offset-beg))))

(defun org-timeline--make-basic-block (task)
  "Make TASK's block and return it as a propertized string.

This does not take the block's context (e.g. overlap) into account."
  (let* ((blank-block (mapconcat 'not (number-sequence 1 24) "      "))
         (id (org-timeline-task-id task))
         (offset-beg (org-timeline-task-offset-beg task))
         (offset-end (org-timeline-task-offset-end task))
         (info (org-timeline-task-info task))
         (face (org-timeline-task-face task))
         (line (org-timeline-task-line-in-agenda-buffer task))
         (group-name (org-timeline-task-group-name task))
         (do-not-overlap (org-timeline-task-do-not-overlap-p task))
         (org-marker (org-timeline-task-org-marker task))
         (org-hd-marker (org-timeline-task-org-hd-marker task))
         (move-to-task-map (let ((x (make-sparse-keymap))
                                 (org-timeline-agenda-todo (lambda ()
                                                             (interactive)
                                                             (ignore-errors
                                                               (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                               (let ((inhibit-quit t))
                                                                 (with-local-quit
                                                                   (org-agenda-todo))
                                                                 (org-timeline--goto-block-position task)))))
                                 (org-timeline-mouse-goto-task-in-file (lambda () (org-timeline--goto-block-position task) (org-timeline--goto-task-in-file)))
                                 (org-timeline-clock-in (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (ignore-errors
                                                              (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                              (let ((inhibit-quit t))
                                                                (with-local-quit
                                                                  (org-agenda-clock-in))
                                                                (org-timeline--goto-block-position task))))))
                                 (org-timeline-clock-out (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (ignore-errors
                                                              (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                              (let ((inhibit-quit t))
                                                                (with-local-quit
                                                                  (org-agenda-clock-out))
                                                                (org-timeline--goto-block-position task))))))
                                 (org-timeline-agenda-redo (lambda ()
                                                             (interactive)
                                                             (let ((pos (point)))
                                                               (goto-line org-timeline-first-line-in-agenda-buffer)
                                                               (forward-line (- org-timeline-height 2))
                                                               (unless (get-text-property (point) 'org-marker)
                                                                 (message "hello there")
                                                                 (beginning-of-buffer))
                                                               (funcall-interactively #'org-agenda-redo)))))
                             (define-key x (kbd "<mouse-1>") #'org-timeline--move-to-task-in-agenda-buffer)
                             (define-key x (kbd "<return>") #'org-timeline--switch-to-task-in-file)
                             (define-key x (kbd "S-<return>") #'org-timeline--move-to-task-in-agenda-buffer)
                             (define-key x (kbd "<tab>") #'org-timeline--goto-task-in-file)
                             (define-key x (kbd "<mouse-2>") #'org-timeline-mouse-goto-task-in-file)
                             (define-key x (kbd "t") org-timeline-agenda-todo)
                             (define-key x (kbd "I") org-timeline-clock-in)
                             (define-key x (kbd "O") org-timeline-clock-out)
                             (define-key x (kbd "r") org-timeline-agenda-redo)
                             x))
         (block-length (- offset-end offset-beg))
         (props (list
                 'org-timeline-occupied t
                 'org-timeline-do-not-overlap do-not-overlap
                 'org-timeline-task-id id
                 'org-timeline-group-name group-name
                 'mouse-face '(:inherit highlight :box t)
                 'keymap move-to-task-map
                 'task-info info
                 'help-echo (lambda (w obj pos) ; called on block hover
                              (org-timeline--draw-new-info w info)
                              info)
                 'cursor-sensor-functions (list #'org-timeline--cursor-sensor-functions)
                 'org-timeline-task-line line
                 'org-marker org-marker
                 'org-hd-marker org-hd-marker
                 ))
         (title (concat org-timeline-insert-before-text
                        (org-timeline-task-text task)
                        blank-block))
         (block (if org-timeline-show-text-in-blocks
                    title
                  blank-block)))
    (add-text-properties 0 block-length props block)
    (add-text-properties 0 block-length (list 'face (seq-filter (lambda (x) (not (functionp x))) face)) block)
    (when-let ((functions (seq-filter #'functionp (reverse face))))
        (dolist (func functions)
          (setq block (funcall func block task))))
    (substring block 0 (+ block-length (- (apply #'+ (mapcar #'char-width org-timeline-insert-before-text)) (length org-timeline-insert-before-text))))))

(defun org-agenda-date-later-minutes (arg)
  "Change the time of this item, in units of `org-time-stamp-rounding-minutes'."
  (interactive "p")
  (undo-auto-amalgamate)
  (setq arg (* arg (cadr org-time-stamp-rounding-minutes)))
  (org-agenda-date-later arg 'minute))
(defun org-agenda-date-earlier-minutes (arg)
  "Change the time of this item, in units of `org-time-stamp-rounding-minutes'."
  (interactive "p")
  (undo-auto-amalgamate)
  (setq arg (* arg (cadr org-time-stamp-rounding-minutes)))
  (org-agenda-date-earlier arg 'minute))
(defun org-agenda-timeline-do-later (&optional arg)
  (interactive "P")
  (atomic-change-group
    (with-undo-amalgamate
      (let* ((message-log-max nil)
             (marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (targ-buffer (marker-buffer marker)))
        (with-temp-message
            (dotimes (x (or arg 1))
              (org-agenda-date-later-minutes 1)))))))
(defun org-agenda-timeline-do-earlier (&optional arg)
  (interactive "P")
  (atomic-change-group
    (with-undo-amalgamate
      (let* ((message-log-max nil)
             (marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (targ-buffer (marker-buffer marker)))
        (with-temp-message
            (dotimes (x (or arg 1))
              (org-agenda-date-earlier-minutes 1)))))))

(defun org-timeline--make-basic-block (task)
  "Make TASK's block and return it as a propertized string.

This does not take the block's context (e.g. overlap) into account."
  (let* ((blank-block (mapconcat 'not (number-sequence 1 24) "      "))
         (id (org-timeline-task-id task))
         (offset-beg (org-timeline-task-offset-beg task))
         (offset-end (org-timeline-task-offset-end task))
         (info (org-timeline-task-info task))
         (face (org-timeline-task-face task))
         (line (org-timeline-task-line-in-agenda-buffer task))
         (group-name (org-timeline-task-group-name task))
         (do-not-overlap (org-timeline-task-do-not-overlap-p task))
         (org-marker (org-timeline-task-org-marker task))
         (org-hd-marker (org-timeline-task-org-hd-marker task))
         (move-to-task-map (let ((x (make-sparse-keymap))
                                 (org-timeline-agenda-todo (lambda ()
                                                             (interactive)
                                                             (ignore-errors
                                                               (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                               (let ((inhibit-quit t))
                                                                 (with-local-quit
                                                                   (org-agenda-todo))
                                                                 (org-timeline--goto-block-position task)))))
                                 (org-timeline-mouse-goto-task-in-file (lambda () (org-timeline--goto-block-position task) (org-timeline--goto-task-in-file)))
                                 (org-timeline-clock-in (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (ignore-errors
                                                              (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                              (let ((inhibit-quit t))
                                                                (with-local-quit
                                                                  (org-agenda-clock-in))
                                                                (org-timeline--goto-block-position task))))))
                                 (org-timeline-clock-out (lambda ()
                                                           (interactive)
                                                           (save-excursion
                                                             (ignore-errors
                                                               (goto-line (org-timeline-task-line-in-agenda-buffer task))
                                                               (let ((inhibit-quit t))
                                                                 (with-local-quit
                                                                   (org-agenda-clock-out))
                                                                 (org-timeline--goto-block-position task))))))
                                 (org-timeline-agenda-redo (lambda ()
                                                             (interactive)
                                                             (let ((pos (point)))
                                                               (goto-line org-timeline-first-line-in-agenda-buffer)
                                                               (forward-line (- org-timeline-height 2))
                                                               (unless (get-text-property (point) 'org-marker)
                                                                 (beginning-of-buffer))
                                                               (funcall-interactively #'org-agenda-redo)
                                                               (goto-char pos))))
                                 (org-timeline-agenda-do-later (lambda (arg)
                                                                 (interactive "P")
                                                                 (save-excursion
                                                                   (beginning-of-buffer)
                                                                   (forward-line (org-timeline-task-line-in-agenda-buffer task))
                                                                   (beginning-of-line)
                                                                   ;; (org-agenda-do-context-action)
                                                                   (let ((inhibit-quit t))
                                                                     (funcall-interactively #'org-agenda-timeline-do-later arg)
                                                                     (org-timeline--goto-block-position task)))))
                                 (org-timeline-agenda-do-earlier (lambda (arg)
                                                                   (interactive "P")
                                                                   (save-excursion
                                                                     (beginning-of-buffer)
                                                                     (forward-line (org-timeline-task-line-in-agenda-buffer task))
                                                                     (beginning-of-line)
                                                                     ;; (org-agenda-do-context-action)
                                                                     (let ((inhibit-quit t))
                                                                       (funcall-interactively #'org-agenda-timeline-do-earlier arg)
                                                                       (org-timeline--goto-block-position task))))))
                             (define-key x (kbd "<mouse-1>") #'org-timeline--move-to-task-in-agenda-buffer)
                             (define-key x (kbd "<return>") #'org-timeline--switch-to-task-in-file)
                             (define-key x (kbd "S-<return>") #'org-timeline--move-to-task-in-agenda-buffer)
                             (define-key x (kbd "<tab>") #'org-timeline--goto-task-in-file)
                             (define-key x (kbd "<mouse-2>") #'org-timeline-mouse-goto-task-in-file)
                             (define-key x (kbd "t") org-timeline-agenda-todo)
                             (define-key x (kbd "I") org-timeline-clock-in)
                             (define-key x (kbd "O") org-timeline-clock-out)
                             (define-key x (kbd "r") org-timeline-agenda-redo)
                             (define-key x (kbd "L") org-timeline-agenda-do-later)
                             (define-key x (kbd "H") org-timeline-agenda-do-earlier)
                             x))
         (block-length (- offset-end offset-beg))
         (props (list
                 'org-timeline-occupied t
                 'org-timeline-do-not-overlap do-not-overlap
                 'org-timeline-task-id id
                 'org-timeline-group-name group-name
                 'mouse-face '(:inherit highlight :box t)
                 'keymap move-to-task-map
                 'task-info info
                 'help-echo (lambda (w obj pos) ; called on block hover
                              (org-timeline--draw-new-info w info)
                              info)
                 'cursor-sensor-functions (list #'org-timeline--cursor-sensor-functions)
                 'org-timeline-task-line line
                 'org-marker org-marker
                 'org-hd-marker org-hd-marker))
         (title (concat org-timeline-insert-before-text
                        (org-timeline-task-text task)
                        blank-block))
         (block (if org-timeline-show-text-in-blocks
                    title
                  blank-block)))
    (add-text-properties 0 block-length props block)
    (add-text-properties 0 block-length (list 'face (seq-filter (lambda (x) (not (functionp x))) face)) block)
    (when-let ((functions (seq-filter #'functionp (reverse face))))
      (dolist (func functions)
        (setq block (funcall func block task))))
    (substring block 0 (+ block-length (- (apply #'+ (mapcar #'char-width org-timeline-insert-before-text)) (length org-timeline-insert-before-text))))))

(defun org-timeline--make-and-insert-block (task)
  "Insert the TASK's block at the right position in the timeline.

Changes the block's face according to context."
  (org-timeline--goto-block-position task)
  (let ((overlapp (not (eq (org-timeline--overlapping-at-point task) nil)))
        (is-next (if (not (eq org-timeline-next-task nil))
                     (eq (org-timeline-task-id task) (org-timeline-task-id org-timeline-next-task))
                   nil))
        (block (org-timeline--make-basic-block task)))
    (when overlapp (setq block (propertize block 'face (list 'org-timeline-overlap 'org-timeline-block 'org-timeline-foreground))))
    (when is-next (setq block (propertize block 'face (list 'org-timeline-next-block 'org-timeline-block 'org-timeline-foreground))))
    (unless (get-text-property (- (point) 1) 'org-timeline-overline)
      (setq block
            (apply #'concat
                   (mapcar (lambda (blo)
                        (unless (= (length blo) 0)
                          (setq blo (propertize blo
                                       'face (append '((:overline t)) (get-text-property 0 'face blo)))))
                        blo)
                      (string-split block ""))))
      (add-text-properties 0 (length block)
                           (list
                            'org-timeline-overline t
                                 ;; 'face (append (get-text-property 0 'face block) '((:overline t)))
                                 'mouse-face (append (get-text-property 0 'mouse-face block) '((:overline t))))
                           block)
      )
    (setq block (substring block 0 (min (length block) (- (line-end-position) (point)))))
    (delete-char (length block))
    (insert block)))

(defun org-timeline--merge-for-24h-cycle ()
  "Kill elapsed columns in day's line according to `org-timeline-keep-elapsed'.

Move tomorrow's line to the right of today's line, to show a complete 24h cycle.
See the documentation of `org-timeline-keep-elapsed' for more information."
  ;; FIXME: quite hacky. This should probably be done directly when making the tasks list,
  ;; maybe by making all those events happen the same fake '0' day and change the offsets accordingly.
  (let* ((today (calendar-absolute-from-gregorian (calendar-current-date)))
         (current-hour (string-to-number (format-time-string "%H")))
         (current-time (+ (* 60 current-hour)
                          (string-to-number (format-time-string "%M"))))
         (elapsed-hours (- (floor (/ current-time 60)) org-timeline-beginning-of-day-hour))
         (number-of-columns-tomorrow (max 0 (- elapsed-hours org-timeline-keep-elapsed)))
         (number-of-columns-today (- 24 number-of-columns-tomorrow))
         (hourline-piece (delete-and-extract-region 6 (+ 6 (* 6 number-of-columns-tomorrow))))
         (today-line-pieces nil)
         (tomorrow-line-pieces nil)
         (cycle-offset (* 6 (- (max org-timeline-beginning-of-day-hour (- current-hour org-timeline-keep-elapsed)) org-timeline-beginning-of-day-hour)))
         (blank-today-line-piece (concat (org-timeline--get-empty-prefix) "  "
                                         (substring (org-timeline--add-elapsed-face org-timeline-slotline)
                                                    cycle-offset
                                                    (+ cycle-offset (* 6 number-of-columns-today)))
                                         "|"))
         (blank-tomorrow-line-piece (concat (org-timeline--get-empty-prefix) "  "
                                            (substring org-timeline-slotline 0 (* 6 number-of-columns-tomorrow)))))
    (goto-char 1)
    (goto-char (line-end-position))
    (insert hourline-piece)
    ;; build (today|tomorrow)-line-pieces lists.
    (while (not (eq (line-end-position) (point-max)))
      (forward-line)
      (let* ((lbeg (line-beginning-position))
             (lend (line-end-position))
             (prefix-length (+ org-timeline-prefix-length 2))
             (today-portion (concat (buffer-substring lbeg (+ lbeg (1- prefix-length)))
                                    (buffer-substring (- lend (* 6 number-of-columns-today) 1) lend)))
             (tomorrow-portion (buffer-substring (+ lbeg prefix-length)
                                                 (+ lbeg prefix-length (* 6 number-of-columns-tomorrow)))))
        (when (eq (get-text-property lbeg 'org-timeline-day) today)
          (setq today-line-pieces (append today-line-pieces (list today-portion))))
        (when (eq (get-text-property lbeg 'org-timeline-day) (+ today 1))
          (setq tomorrow-line-pieces (append tomorrow-line-pieces (list tomorrow-portion))))))
    ;; handle groups and balance lines
    ;; FIXME: not efficient, doesn't jump once group done
    ;; (print "today")
    ;; (dolist (line today-line-pieces) (print line))
    ;; (print "tomorrow")
    ;; (dolist (line tomorrow-line-pieces) (print line))
    (let (groups-handled)
      (dotimes (i (length today-line-pieces))
        (let* ((group-handled (get-text-property 0 'org-timeline-group-name (seq-elt today-line-pieces i)))
               (group-handled-p (lambda (piece) (string= (get-text-property 1 'org-timeline-group-name piece) group-handled)))
               (prev-pieces-today (seq-take today-line-pieces i))
               (next-pieces-today (seq-drop today-line-pieces i))
               (same-group-pieces-today (seq-filter group-handled-p next-pieces-today))
               (rest-of-pieces-today    (seq-remove group-handled-p next-pieces-today))
               (prev-pieces-tomorrow (seq-take tomorrow-line-pieces i))
               (next-pieces-tomorrow (seq-drop tomorrow-line-pieces i))
               (same-group-pieces-tomorrow (seq-filter group-handled-p next-pieces-tomorrow))
               (rest-of-pieces-tomorrow    (seq-remove group-handled-p next-pieces-tomorrow)))
          ;; balance groups
          (let* ((line-diff (- (length same-group-pieces-tomorrow) (length same-group-pieces-today)))
                 (number-of-blank-lines-to-add-today (max 0 line-diff))
                 (number-of-blank-lines-to-add-tomorrow (max 0 (- 0 line-diff))))
            (dotimes (n number-of-blank-lines-to-add-today)
              (setq same-group-pieces-today (append same-group-pieces-today (list blank-today-line-piece))))
            (dotimes (n number-of-blank-lines-to-add-tomorrow)
              (setq same-group-pieces-tomorrow (append same-group-pieces-tomorrow (list blank-tomorrow-line-piece)))))
          ;; rebuild the pieces lists
          (setq today-line-pieces (append prev-pieces-today same-group-pieces-today rest-of-pieces-today))
          (setq tomorrow-line-pieces (append prev-pieces-tomorrow same-group-pieces-tomorrow rest-of-pieces-tomorrow))))
        (let* ((unhandled-groups-tomorrow (seq-drop tomorrow-line-pieces (length today-line-pieces))))
          (dolist (piece unhandled-groups-tomorrow)
            (if (member (get-text-property 0 'org-timeline-group-name piece) groups-handled)
                (setq today-line-pieces (append today-line-pieces (list blank-today-line-piece)))
              (setq today-line-pieces (append today-line-pieces (list (concat (get-text-property 0 'org-timeline-group-name piece)
                                                                              (substring blank-today-line-piece org-timeline-prefix-length nil))))))
            (push (get-text-property 0 'org-timeline-group-name piece) groups-handled))))
    ;; (print "today")
    ;; (dolist (line today-line-pieces) (print line))
    ;; (print "tomorrow")
    ;; (dolist (line tomorrow-line-pieces) (print line))
    ;; insert them
    (goto-char 1)
    (let ((hourline (buffer-substring 1 (line-end-position))))
      (delete-region (point-min) (point-max))
      (insert hourline))
    (dolist (piece today-line-pieces)
      (insert "\n" piece))
    (goto-line 2)
    (dolist (piece tomorrow-line-pieces)
      (goto-char (line-end-position))
      (insert piece)
      (forward-line))
    ;; remove elapsed face from tomorrow lines
    (goto-char 1)
    (put-text-property (+ org-timeline-prefix-length 2  (* 6 number-of-columns-today)) (line-end-position) 'face nil)
    (while (and (eq (forward-line) 0)
                (not (eq (point) (point-max))))
      (forward-char (+ org-timeline-prefix-length 2 (* 6 number-of-columns-today)))
      (dotimes (i (- (line-end-position) (point)))
        (when (not (get-text-property (point) 'org-timeline-occupied))
          (put-text-property (point) (+ (point) 1) 'face nil))
        (forward-char)))))

;; Some ideas for the the generation of the timeline were inspired by the
;; forked repo: https://github.com/deopurkar/org-timeline.
(defun org-timeline--generate-timeline ()
  "Generate the timeline string that will represent current agenda view."
  (let* ((hourline (concat (org-timeline--get-empty-prefix) " "
                           (org-timeline--add-elapsed-face
                            (concat "|"
                                    (mapconcat (lambda (x) (format "%02d:00" (mod x 24)))
                                               (number-sequence org-timeline-beginning-of-day-hour (+ org-timeline-beginning-of-day-hour 23))
                                               "|")
                                    "|"))))
         (tasks (org-timeline--list-tasks))
         (today (calendar-absolute-from-gregorian (calendar-current-date)))
         (today-onlyp (eq 0 (length (delq nil (mapcar (lambda (task) (if (eq (org-timeline-task-day task) today) nil task)) tasks)))))
         (today-or-tomorrow-only-p (eq 0 (length (delq nil (mapcar (lambda (task) (if (member (org-timeline-task-day task) `(,today ,(+ today 1))) nil task)) tasks))))))
    (with-temp-buffer
      (insert hourline)
      (dolist (task tasks)
        ;; (print (buffer-substring (point-min) (point-max)))
        (org-timeline--make-and-insert-block task))
      ;; (print (buffer-substring (point-min) (point-max)))
      (when (and (>= org-timeline-keep-elapsed 0)
                 today-or-tomorrow-only-p
                 (> (length tasks) 0))
        (org-timeline--merge-for-24h-cycle))
      ;; display the next block's info
      (goto-char (point-max))
      (unless (eq (length tasks) 0)
        (insert "\n"
                (if (eq org-timeline-next-task nil)
                    (propertize "  no incoming event" 'org-timeline-info-line t)
                  (org-timeline--decorate-info (org-timeline-task-info org-timeline-next-task)))))
      (buffer-string))))

;;;###autoload
(defun org-timeline-insert-timeline ()
    "Insert graphical timeline into agenda buffer."
    ;; (message "Inserting org-timeline...")
    (when org-timeline-cursor-sensor (cursor-sensor-mode 1))
    (unless (buffer-narrowed-p)
      (goto-char (point-min))
      (unless org-timeline-prepend
        (while (and (eq (get-text-property (line-beginning-position) 'org-agenda-type) 'agenda)
                    (not (eobp)))
          (forward-line)))
      (forward-line)
      (let ((inhibit-read-only t))
        (setq org-timeline-first-line-in-agenda-buffer (line-number-at-pos))
        (insert (propertize (concat (make-string (window-width) ?─)) 'face 'org-time-grid) "\n")
        (insert (org-timeline--generate-timeline))
        (insert (propertize (concat "\n" (make-string (window-width) ?─)) 'face 'org-time-grid 'org-timeline-end t) "\n")
        (setq org-timeline-height (- (line-number-at-pos) org-timeline-first-line-in-agenda-buffer)))
      ;; enable `font-lock-mode' in agenda view to display the "chart"
      ;; Note: I have local changes replacing this with propertization, need to integrate into the file.
      ;; (let ((font-lock-unfontify-region-function #'ignore)
      ;;       (font-lock-unfontify-buffer-function #'ignore))
      ;;   (message "unfontify %s %s"
      ;;            font-lock-unfontify-region-function
      ;;            font-lock-unfontify-buffer-function)
      ;;  (font-lock-mode 1))

      (goto-char (point-max))
      (beginning-of-line)
      (while (not (or (get-text-property (point) 'org-timeline-info-line)
                      ;; (get-text-property (point) 'org-timeline-day)
                      (<= (point) 5)))
        (forward-line -1)
        (beginning-of-line)))
    ;; (message "Inserting org-timeline...done")
    )

(provide 'org-timeline)
;;; org-timeline.el ends here
