;;; goto-line-preview.el --- Preview line when executing `goto-line` command.    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-03-01 14:53:00

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Preview line when executing `goto-line` command.
;; Keyword: line navigation
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/jcs090218/goto-line-preview

;; This file is NOT part of GNU Emacs.

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
;;
;; Preview line when executing `goto-line` command.
;;

;;; Code:


(defgroup goto-line-preview nil
  "Preview line when executing `goto-line` command."
  :prefix "goto-line-preview-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/goto-line-preview"))


(defvar goto-line-preview-prev-buffer nil
  "Record down the previous buffer before we do `goto-line-preview-goto-line' command.")

(defvar goto-line-preview-prev-line-num nil
  "Record down the previous line number before we do `goto-line-preview-goto-line' command.")


(defun goto-line-preview-do-preview ()
  "Do the goto line preview action."
  (save-selected-window
    (when goto-line-preview-prev-buffer
      (let ((line-num-str (thing-at-point 'line)))

        (switch-to-buffer goto-line-preview-prev-buffer)

        (if line-num-str
            (let ((line-num (string-to-number line-num-str)))
              (unless (zerop line-num)
                (goto-line-preview-do line-num)))
          (goto-line-preview-do goto-line-preview-prev-line-num))))))

(defun goto-line-preview-do (line-num)
  "Do goto line.
LINE-NUM : Target line number to navigate to."
  (save-selected-window
    (switch-to-buffer goto-line-preview-prev-buffer)
    (with-no-warnings
      (goto-line line-num))
    (call-interactively #'recenter)))


;;;###autoload
(defun goto-line-preview-goto-line ()
  "Preview goto line.
LINE-NUM : Target line number to navigate to."
  (interactive)
  (let ((window (selected-window))
        (window-point (window-point))
        jumped)
    (unwind-protect
        (let ((goto-line-preview-prev-buffer (buffer-name))
              (goto-line-preview-prev-line-num (line-number-at-pos)))
          (setq jumped (read-number "Goto line: ")))
      (unless jumped
        (message "unwind")
        (set-window-point window window-point)))))


(defun goto-line-preview-minibuffer-setup ()
  "Locally set up preview hooks for this minibuffer command."
  (when (eq 'goto-line-preview-goto-line this-command)
    (add-hook 'post-command-hook
              #'goto-line-preview-do-preview nil t)))

(add-hook 'minibuffer-setup-hook 'goto-line-preview-minibuffer-setup)



(provide 'goto-line-preview)
;;; goto-line-preview.el ends here
