;;; indent-guide.el --- show vertical lines to guide indentation

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 2.0.2

;;; Commentary:

;; Require this script
;;
;;   (require 'indent-guide)
;;
;; and call command "indent-guide-mode".

;; If you want to enable indent-guide-mode in all buffers,
;; set the default value of "indent-guide-mode" non-nil.
;;
;;   (setq-default indent-guide-mode t)
;;
;; in your init file.

;; Column lines are propertized with "indent-guide-face". So you may
;; configure this face to make guides more pretty in your colorscheme.
;;
;;   (set-face-background 'indent-guide-face "dimgray")
;;
;; You may also change the character for guides.
;;
;;   (setq indent-guide-char ":")

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 cleaned and optimized code
;;       works better for the file without trailing-whitespaces
;; 1.0.2 modified behavior for lines with only whitespaces
;; 1.0.3 Allow custom indent guide char
;; 1.0.4 disabled in org-indent-mode
;; 1.0.5 faster update of indent-guide (especially for huge files)
;; 1.1.0 work with tab-indented files
;; 1.1.1 turned into minor-mode
;; 1.1.2 an infinite-loop bug fix
;; 1.1.3 changed behavior for blank lines
;; 2.0.0 rewrite almost everything
;; 2.0.1 improve blank-line and tab handling
;; 2.0.2 fixed bug that sometimes newline gets invisible

;;; Code:

(defconst indent-guide-version "2.0.2")

;; * customs

(defgroup indent-guide nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indent-guide-char "|"
  "character used as vertical line"
  :group 'indent-guide)

;; * minor-mode

(defvar indent-guide-mode nil)
(make-variable-buffer-local 'indent-guide-mode)

(defun indent-guide-mode (&optional arg)
  (interactive)
  (setq indent-guide-mode (if arg (< arg 0)
                            (not indent-guide-mode)))
  (message (if indent-guide-mode
               "indent-guide-mode enabled"
             "indent-guide-mode disabled")))

(when (not (assq 'indent-guide-mode minor-mode-alist))
  (setq minor-mode-alist (cons '(indent-guide-mode " Ingd") minor-mode-alist)))

;; * variables / faces

(make-face 'indent-guide-face)
(set-face-attribute 'indent-guide-face nil
                    :foreground "#535353")

;; * utilities

(defun indent-guide--active-overlays ()
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-guide) ov))
         (overlays-in (point-min) (point-max)))))

;; * generate guides

(defun indent-guide--draw-line (col)
  "draw \"indent-guide-char\" at the COLUMN in this line"
  (save-excursion
    (move-to-column col)
    (let ((diff (- (current-column) col))
          string ov)
      (cond ((eolp)                     ; blank line (with or without indent)
             (setq string (concat (make-string (- diff) ?\s)
                                  indent-guide-char))
             (setq ov (make-overlay (point) (point))))
            ((not (zerop diff))         ; looking back tab (unexpectedly)
             (setq string (concat (make-string (- tab-width diff) ?\s)
                                  indent-guide-char
                                  (make-string (1- diff) ?\s)))
             (setq ov (make-overlay (1- (point)) (point))))
            ((looking-at "\t")          ; looking at tab
             (setq string (concat indent-guide-char
                                  (make-string (1- tab-width) ?\s)))
             (setq ov (make-overlay (point) (1+ (point)))))
            (t                          ; no problem
             (setq string indent-guide-char)
             (setq ov (make-overlay (point) (1+ (point))))))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'category 'indent-guide)
      (overlay-put ov 'before-string
                   (propertize string 'face 'indent-guide-face)))))

(defun indent-guide-show ()
  (when indent-guide-mode
   (unless (or (indent-guide--active-overlays)
               (active-minibuffer-window))
     (save-excursion
       (let ((start (window-start))
             (end (window-end))
             (ind-col (progn (back-to-indentation) (current-column)))
             line-col)
         (unless (zerop ind-col)
           ;; search column
           (while (and (zerop (forward-line -1))
                       (progn (back-to-indentation) t)
                       (or (<= ind-col (current-column)) (eolp))))
           (setq line-col (current-column))
           ;; draw line
           (while (and (zerop (forward-line 1))
                       (< (point) start)))
           (while (and (progn (back-to-indentation) t)
                       (or (< line-col (current-column)) (eolp))
                       (indent-guide--draw-line line-col)
                       (progn (forward-line 1) (not (eobp)))
                       (<= (point) end)))))))))

(defun indent-guide-remove ()
  (when indent-guide-mode
    (dolist (ov (indent-guide--active-overlays))
      (delete-overlay ov))))

;; * triggers

(add-hook 'pre-command-hook 'indent-guide-remove)
(add-hook 'post-command-hook 'indent-guide-show)

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
