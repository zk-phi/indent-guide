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
;; Version: 1.1.0

;;; Commentary:

;; Require this script
;;
;;   (require 'indent-guide)
;;
;; then indent-guide appears automatically.

;; To set delay until the indent-guide appears, use function
;; "indent-guide-set-delay".
;;
;;   (indent-guide-set-delay 1.0)
;;
;; Now indent-guide appears after 1.0 sec of idle time.

;; Column lines are applied "indent-guide-face". So you may configure
;; this face to make liens more pretty in your colorscheme.
;;
;;   (set-face-background 'indent-guide-face "dimgray")

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 cleaned and optimized code
;;       works better for the file without trailing-whitespaces
;; 1.0.2 modified behavior for lines with only whitespaces
;; 1.0.3 Allow custom indent guide char
;; 1.0.4 disabled in org-indent-mode
;; 1.0.5 faster update of indent-guide (especially for huge files)
;; 1.1.0 work with tab-indented files

;;; Known limitations, bugs:

;; o works not perfectly with "hl-line".

;;; Code:

(defconst indent-guide-version "1.1.0")

;; * customs

(defgroup indent-guide nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indent-guide-char "|"
  "character used as vertical line"
  :group 'indent-guide)

;; * variables / faces

(make-face 'indent-guide-face)
(set-face-attribute 'indent-guide-face nil
                    :foreground "#535353")

(defvar indent-guide-timer-object
  (run-with-idle-timer 0.6 t 'indent-guide-update))

(defun indent-guide-set-delay (sec)
  "change delay until the indent-guide appears"
  (timer-set-idle-time indent-guide-timer-object
                       sec t))

;; * utilities

(defun indent-guide--diff-list (list)
  "1 5 10 -> 4 5 / 1 5 nil -> 4 1
if the last element is nil, then the last diff bocomes 1"
  (let* ((res nil)
         (prev (car list))
         (list (cdr list)))
    (while list
      (setq res (cons (if (car list) (- (car list) prev) 1)
                      res)
            prev (car list)
            list (cdr list)))
    (reverse res)))

(defun indent-guide--snoc (elem list)
  "reversed cons"
  (reverse (cons elem (reverse list))))

(defun indent-guide--beginning-of-block ()
  "go to the beginning of this indent block"
  (or (search-backward-regexp "^[^\s\t\n]" nil t)
      (goto-char (point-min))))

(defun indent-guide--end-of-block ()
  "go to the end of this indent block"
  (or (and (ignore-errors (forward-char) t)
           (or (search-forward-regexp "^[^\s\t\n]" nil t)
               (goto-char (point-max)))
           (or (search-backward-regexp "^[\s\t]" nil t)
               (goto-char (point-min)))
           (progn (end-of-line) (point)))
      (goto-char (point-max))))

(defun indent-guide--active-overlays (beg end)
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-guide) ov))
         (overlays-in beg end))))

;; * generate guides

(defun indent-guide--region->indents (beg end)
  "make a list of column numbers that represents indentations."
  (save-excursion
    (let ((res nil))
      (goto-char beg)
      (while (and (not (eobp)) (< (point) end))
        (back-to-indentation)
        (setq res (cons (and (not (eolp)) (current-column))
                        res))
        (vertical-motion 1))
      (reverse res))))

(defun indent-guide--indents->guides (indents)
  "take a list of indentations, and returns list of guides"
  (let ((active-guides nil)
        (res nil))
    (dolist (indent indents)
      ;; if indent is non-nil, filter active-guides
      (when indent
        (setq active-guides
              (delq nil
                    (mapcar (lambda (x) (and (< x indent) x))
                            active-guides))))
      ;; calc result
      (setq res (cons (indent-guide--diff-list
                       (indent-guide--snoc indent (reverse active-guides)))
                      res))
      ;; update active-guides
      (when indent
        (setq active-guides (cons indent active-guides))))
    (reverse res)))

(defun indent-guide--guides->strings (guides)
  "(2 1 1) -> \"| ||\""
  (let ((res nil))
    (dolist (guide guides)
      (setq res
            (cons (mapconcat (lambda (x) (concat "|" (make-string (1- x) ?\s)))
                             guide "")
                  res)))
    (reverse res)))

(defun indent-guide--get-strings (beg end)
  (indent-guide--guides->strings
   (indent-guide--indents->guides
    (indent-guide--region->indents beg end))))

;; * show or hide indent-guides

(defun indent-guide-remove (beg end)
  (dolist (ov (indent-guide--active-overlays beg end))
    (delete-overlay ov)))

(defun indent-guide-show (beg end)
  ;; if overlays are already active, do nothing
  (unless (indent-guide--active-overlays beg end)
    (save-excursion
      (let ((strings (indent-guide--get-strings beg end))
            (wstart (window-start))
            (wend (window-end)))
        (goto-char beg)
        ;; skip lines before window-start
        (while (< (point) wstart)
          (setq strings (cdr strings))
          (vertical-motion 1))
        ;; draw indent-guide
        (while (and strings (< (point) wend))
          (back-to-indentation)
          (let ((ov (make-overlay (point-at-bol) (point))))
            (overlay-put ov 'invisible t)
            (overlay-put ov 'category 'indent-guide)
            (overlay-put ov 'before-string
                         (propertize (car strings) 'face 'indent-guide-face)))
          (setq strings (cdr strings))
          (vertical-motion 1))))))

;; * triggers

(defun indent-guide-update ()
  (interactive)
  (unless (or (and (boundp 'org-indent-mode) org-indent-mode)
              (active-minibuffer-window))
    ;; back-to-indentation (if appropriate)
    (let ((point (save-excursion (back-to-indentation)
                                 (point))))
      (when (< (point) point) (goto-char point)))
    ;; draw indent-guide
    (save-excursion
      (ignore-errors (forward-char))
      (indent-guide-show (indent-guide--beginning-of-block)
                         (indent-guide--end-of-block)))))

(defun indent-guide-pre-command ()
  (save-excursion
    ;; (ignore-errors (forward-char))        ; *FIXME*
    ;; (let* ((beg (indent-guide-beginning-of-defun))
    ;;        (end (indent-guide-end-of-defun)))
    ;;   (indent-guide-remove beg end))
    (indent-guide-remove (point-min) (point-max))))

(add-hook 'pre-command-hook 'indent-guide-pre-command)

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here