;;; goto-line-preview.el --- Preview line when executing `goto-line` command    -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Shen, Jen-Chieh
;; Created date 2019-03-01 14:53:00

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/goto-line-preview
;; Version: 0.1.1
;; Package-Requires: ((emacs "25"))
;; Keywords: convenience line navigation

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
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/goto-line-preview"))

(defcustom goto-line-preview-before-hook nil
  "Hooks run before `goto-line-preview' is run."
  :group 'goto-line-preview
  :type 'hook)

(defcustom goto-line-preview-after-hook nil
  "Hooks run after `goto-line-preview' is run."
  :group 'goto-line-preview
  :type 'hook)

(defvar goto-line-preview--prev-window nil
  "Record down the previous window before we do preivew display.")

(defvar goto-line-preview--prev-line-num nil
  "Record down the previous line number before we do preivew display.")

(defvar goto-line-preview--relative-p nil
  "Flag to see if this command relative.")

(defun goto-line-preview--do (line-num)
  "Do goto LINE-NUM."
  (save-selected-window
    (select-window goto-line-preview--prev-window)
    (goto-char (point-min))
    (forward-line (1- line-num))))

(defun goto-line-preview--do-preview ()
  "Do the goto line preview action."
  (save-selected-window
    (when goto-line-preview--prev-window
      (let ((line-num-str (thing-at-point 'line)))
        (select-window goto-line-preview--prev-window)
        (if line-num-str
            (let ((line-num (string-to-number line-num-str)))
              (when goto-line-preview--relative-p
                (setq line-num (+ goto-line-preview--prev-line-num line-num)))
              (unless (zerop line-num) (goto-line-preview--do line-num)))
          (goto-line-preview--do goto-line-preview--prev-line-num))))))

;;;###autoload
(defun goto-line-preview ()
  "Preview goto line."
  (interactive)
  (let ((goto-line-preview--prev-window (selected-window))
        (window-point (window-point))
        (goto-line-preview--prev-line-num (line-number-at-pos))
        jumped)
    (run-hooks 'goto-line-preview-before-hook)
    (unwind-protect
        (setq jumped (read-number
		      (let ((lines (line-number-at-pos (point-max))))
			(format (if goto-line-preview--relative-p
				    "[%d] Goto line relative: (%d to %d) "
				  "[%d] Goto line: (%d to %d) ")
				goto-line-preview--prev-line-num
				(max 0 (min 1 lines))
				lines))))
      (if jumped
          (with-current-buffer (window-buffer goto-line-preview--prev-window)
            (unless (region-active-p) (push-mark window-point)))
        (set-window-point goto-line-preview--prev-window window-point))
      (run-hooks 'goto-line-preview-after-hook))))

;;;###autoload
(defun goto-line-preview-relative ()
  "Preview goto line relative."
  (interactive)
  (let ((goto-line-preview--relative-p t))
    (goto-line-preview)))

;;;###autoload
(define-obsolete-function-alias 'goto-line-preview-goto-line 'goto-line-preview "0.1.1")

(defun goto-line-preview--minibuffer-setup ()
  "Locally set up preview hooks for this minibuffer command."
  (when (memq this-command '(goto-line-preview
                             goto-line-preview-goto-line
                             goto-line-preview-relative))
    (add-hook 'post-command-hook #'goto-line-preview--do-preview nil t)))

(add-hook 'minibuffer-setup-hook 'goto-line-preview--minibuffer-setup)

(provide 'goto-line-preview)
;;; goto-line-preview.el ends here
