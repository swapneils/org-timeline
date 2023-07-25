# org-timeline [![Build Status](https://travis-ci.org/Fuco1/org-timeline.svg?branch=master)](https://travis-ci.org/Fuco1/org-timeline)

Add graphical view of agenda to agenda buffer.

![Preview](./img/timeline2.png)

# Installation

After you install this package from MELPA Stable, add the following line to your org configuration:

``` emacs-lisp
(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
```

Also, here are some reasonable defaults for configuration options:

``` emacs-lisp
(setq org-timeline-overlap-in-new-line t
     org-timeline-emphasize-next-block t
     org-timeline-beginning-of-day-hour 0
     org-timeline-keep-elapsed -1
     org-agenda-span 2
     org-timeline-show-text-in-blocks t
     org-timeline-insert-before-text "\u25B6"
     org-timeline-emphasize-priority 'c
     )
(set-face-attribute 'org-timeline-block nil :background "#5555BB")
```

`

# How it works

This package adds a graphical view of the agenda after the last agenda line.  By default the display starts at 5 AM today and goes up to 4 AM next day (this covers 24 hours). This value can be customized with `org-timeline-beginning-of-day-hour`.

Scheduled tasks or tasks with time ranges are rendered in the display with `org-timeline-block` face.  Clocked entries are displayed in `org-timeline-clocked` face.  The background of timeslots which are in the past is highlighted with `org-timeline-elapsed` face.

You can use custom color for a task by adding the property `TIMELINE_FACE` with either a string which is a color name or a list which specifies the face properties or a symbol which is taken to be a face name.
You can choose to show the task's headlines in blocks, by setting `org-timeline-show-text-in-blocks` to a non-nil value. You can customize the text for a task with by adding the property `TIMELINE_TEXT` with a string.

# Further customization

## Overlapping blocks
By default, if two blocks overlap, one of them is drawn in the `org-timeline-overlap` face. You can set `org-timeline-overlap-in-new-line` to t, and overlapping blocks will be drawn in separate lines.
You can also be task-specific, and add the property `TIMELINE_DO_NOT_OVERLAP` with a non-nil value.

## Consecutive blocks
In order to make consecutive blocks distinct, every other consecutive block is decorated with a white overline. 

For the same reason, a character is added at the beginning of every block, if `org-timeline-show-text-in-blocks` is non-nil. By default, this character is a heavy vertical bar ❚, but it can be customized with `org-timeline-insert-before-text`.

## Special entries
By default, clocked entries will be shown in a dedicated line, in `org-timeline-clocked` face. If you do not like this, you can set `org-timeline-dedicated-clocked-line` to nil. 

You can also emphasize the next block to happen with `org-timeline-emphasize-next-block`. If non-nil, the next-block in today's line will be drawn in `org-timeline-next-block` face.

## Groups
You can add the string property `TIMELINE_GROUP` to your tasks. Every task with the same group name will be shown in a separate, dedicated line for that day.
The first three characters of the name will be shown at the beginning of that line.

## Rolling 24h cycle
You can set up org-timeline and org-agenda so that the timeline will show a rolling 24h cycle, starting a certain number of hours before now.
- Set `org-agenda-span` to 2 
- Set `org-timeline-beginning-of-day-hour` to 0 
- Set `org-timeline-keep-elapsed` to a positive integer (5, for example).

Run `org-agenda` in day mode.

## Priority-based highlighting
By default, blocks are given different colors if they have been given an explicit priority cookie (e.g. "[#A]"). You can change the value `org-timeline-emphasize-priority` to either 'a, 'b, 'c, or a number, to indicate a minimum priority value (below which priority-based coloring will be discarded), or set it to `nil` to deactivate this feature.

# Other details
The info line (just below the timeline) by default shows the details of the next task to happen, but changes to the most recent block you hovered over. You can hit 'r' outside of the timeline to refresh the agenda and show the next task again.

# TODO

- [x] Add faces instead of colors
- [X] Make "midnight"/change-of-day configurable (currently 5 AM)
- [X] Add a tooltip showing the task description/name
- [X] Make the blocks navigable to the task


# My org-timeline config
(This config is implemented in Doom Emacs, but is hopefully clear enough to port to other systems)

``` emacs-lisp
(use-package! org-timeline
  :after org-agenda
  :config
  (defun org-timeline-get-priority-face (task)
    (if-let ((value-matches (mapcar (lambda (c) (cons (cdr c) (car c))) org-timeline-priority-matches))
             (curr-priority (when (string-match org-priority-regexp (org-timeline-task-info task))
                              (org-get-priority (org-timeline-task-info task))))
             (curr-priority-name (alist-get curr-priority value-matches))
             (curr-priority-face (face-attribute (org-get-priority-face (upcase (string-to-char (symbol-name curr-priority-name))))
                                                 :foreground)))
        curr-priority-face))
  (defface org-timeline-faded-face '((t :background "#373b45" :foreground "#575b65"))
    "Face for fading out blocks")
  (custom-set-faces!
    ;; (list 'org-timeline-clocked-default-task-face (face-default-spec 'org-timeline-clocked))
    (list 'org-timeline-clocked '(t (:background "#626"))))
  (setq! ;; org-timeline-start-hour 0
   org-timeline-overlap-in-new-line t
   org-timeline-emphasize-next-block t
   org-timeline-beginning-of-day-hour 0
   org-timeline-keep-elapsed -1
   org-agenda-span 2
   org-timeline-show-text-in-blocks t
   org-timeline-insert-before-text "\u25B6"
   org-timeline-emphasize-priority 0
   org-timeline-show-clocked t
   org-timeline-dedicated-clocked-line t
   org-timeline-cursor-sensor t
   org-timeline-default-duration 10
   org-timeline-tag-faces (list
                           "defaultclock" (lambda (bl task)
                                            (when-let ((bl-face (get-text-property 0 'font-lock-face bl))
                                                       (clocked-p (member 'org-timeline-clocked bl-face)))
                                              (add-text-properties 0 (length bl)
                                                                   (list 'font-lock-face
                                                                      (cons 'org-timeline-faded-face bl-face))
                                                                   bl))
                                            bl)
                           "meeting" (lambda (bl task)
                                       (let ((orig-foreground (with-temp-buffer
                                                     (insert bl)
                                                     (goto-char 0)
                                                     (foreground-color-at-point))))
                                         (add-text-properties 0 (length bl)
                                                              (list 'font-lock-face
                                                                 (cons '(:weight bold :background "#000" :foreground "#fff") (get-text-property 0 'font-lock-face bl)))
                                                              bl)
                                         (if-let ((curr-priority-face (org-timeline-get-priority-face task)))
                                          (add-text-properties 0 1
                                                               (list 'font-lock-face
                                                                  (cons `(:foreground ,curr-priority-face)
                                                                        (get-text-property 0 'font-lock-face bl)))
                                                               bl)))
                                       ;; (let* ((orig (get-text-property 0 'font-lock-face bl))
                                       ;;       (start-back (plist-get orig :background)))
                                       ;;   (message "orig %s" orig)
                                       ;;   (add-text-properties 0 (length bl)
                                       ;;                        (list 'font-lock-face
                                       ;;                           (cons '(:weight bold :background "#000" :foreground "#fff") (get-text-property 0 'font-lock-face bl)))
                                       ;;                        bl)
                                       ;;   (when start-back
                                       ;;     (message "start-back %s")
                                       ;;     (add-text-properties 0 1
                                       ;;                          (list 'font-lock-face
                                       ;;                             (cons (list ':foreground start-back) (get-text-property 0 'font-lock-face bl)))
                                       ;;                          bl)))
                                       bl)))
  (defun org-agenda-timeline-todo (&optional arg)
    (if (get-text-property)))
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
  (custom-set-faces!
    '(org-timeline-block :background "#5555BB")
    '(org-timeline-next-block
      :background "#050"
      :foreground "#0ff"
      :weight bold)
    '(org-timeline-clocked :background "#333" :foreground "#fdd"))

  ;; Doesn't work
  ;; (after! better-jumper
  ;;   (defadvice! org-timeline--move-to-task-in-agenda-push-to-better-jumper (&rest _)
  ;;     :before #'org-timeline--move-to-task-in-agenda-buffer
  ;;     (better-jumper--push)))
  )
```

