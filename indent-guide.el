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
;; Version: 1.0.4

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

;;; Known limitations, bugs:

;; o works not perfectly with "hl-line".

;;; Code:

(defconst indent-guide-version "1.0.4")

;; * customs

(defgroup indent-guide nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indent-guide-char "|"
  "character used as vertical line"
  :group 'indent-guide)

;; * variables / faces

(defvar indent-guide-timer-object
  (run-with-idle-timer 0.6 t 'indent-guide-update))

(make-face 'indent-guide-face)
(set-face-attribute 'indent-guide-face nil
                    :foreground "#535353")

(defun indent-guide-set-delay (sec)
  "change delay until the indent-guide appears"
  (timer-set-idle-time indent-guide-timer-object
                       sec t))

;; * private functions

(defun indent-guide--indent-list (beg end)
  (save-excursion
    (let ((lst nil))
      (goto-char end)
      (beginning-of-line)
      (while (< beg (point))
        (setq lst (cons
                   (if (eolp) nil
                     (progn (back-to-indentation) (current-column)))
                   lst))
        (vertical-motion -1))
      (back-to-indentation)
      (setq lst (cons (current-column) lst)))))

(defun indent-guide--guide-strings (indent-list)
  (flet ((set-nth (n lst val)
                  (if (zerop n)
                      (setcar lst val)
                    (set-nth (1- n) (cdr lst) val)))
         (guides (indent-list)
                 (let ((active nil)
                       (coming indent-list)
                       (guides nil))
                   (dotimes (n (length indent-list))
                     (let ((current (car coming)))
                       (if (null current)
                           (setq guides (cons active guides))
                         (setq active (delq nil
                                            (mapcar (lambda (x) (and (< x current) x))
                                                    active)))
                         (setq guides (cons active guides))
                         (add-to-list 'active current t))
                       (setq coming (cdr coming))))
                   (reverse guides)))
         (guide-string (guide)
                       (if (null guide) ""
                         (let* ((length (1+ (eval `(max ,@guide))))
                                (str (make-list length " ")))
                           (dolist (n guide)
                             (set-nth n str indent-guide-char))
                           (eval `(concat ,@str))))))
    (let ((guides (guides indent-list)))
      (mapcar 'guide-string guides))))

;; * show or hide indent-guides

(defun indent-guide-overlays (beg end)
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-guide) ov))
         (overlays-in beg end))))

(defun indent-guide-show (beg end)
  (unless (indent-guide-overlays beg end)
    (save-excursion
      (let* ((indent-list (indent-guide--indent-list beg end))
             (string-list (indent-guide--guide-strings indent-list))
             ov)
        (goto-char beg)
        (dotimes (n (1- (length indent-list)))
          (setq indent-list (cdr indent-list)
                string-list (cdr string-list))
          (cond ((null (car indent-list))
                 (vertical-motion 1)
                 (if (eolp)
                     (setq ov (make-overlay (point) (1+ (point))))
                   (setq ov (make-overlay (point) (point-at-eol)))
                   (overlay-put ov 'invisible t)))
                (t
                 (vertical-motion (cons (length (car string-list)) 1))
                 (setq ov (make-overlay (point-at-bol) (point)))
                 (overlay-put ov 'invisible t)))
          (overlay-put ov 'category 'indent-guide)
          (overlay-put ov 'before-string
                       (propertize (car string-list) 'face 'indent-guide-face)))))))

(defun indent-guide-remove (beg end)
  (dolist (ov (indent-guide-overlays beg end))
    (delete-overlay ov)))

;; * triggers

(defun indent-guide-beginning-of-defun ()
  (or (search-backward-regexp "^[^\s\t\n]" nil t)
      (goto-char (point-min))))

(defun indent-guide-end-of-defun ()
  (or (and (ignore-errors (forward-char) t)
           (or (search-forward-regexp "^[^\s\t\n]" nil t)
               (goto-char (point-max)))
           (or (search-backward-regexp "^[\s\t]" nil t)
               (goto-char (point-min)))
           (progn (end-of-line) (point)))
      (goto-char (point-max))))

(defun indent-guide-update ()
  (unless (or (and (boundp 'org-indent-mode) org-indent-mode)
              (active-minibuffer-window))
    (save-excursion
      (ignore-errors (forward-char))        ; *FIXME*
      (let* ((beg (indent-guide-beginning-of-defun))
             (end (indent-guide-end-of-defun)))
        (indent-guide-show beg end)))))

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
