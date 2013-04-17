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
;; Version: 1.0.0

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

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(defconst indent-guide-version "1.0.0")

;; * variables / faces

(defvar indent-guide-timer-object
  (run-with-idle-timer 0.6 t 'indent-guide-update))

(make-face 'indent-guide-face)
(set-face-attribute 'indent-guide-face nil
                    :foreground "#555555")

(defun indent-guide-set-delay (sec)
  "change delay until the indent-guide appears"
  (timer-set-idle-time indent-guide-timer-object
                       sec t))

;; * show or hide indent-guides

(defun indent-guide-overlays (beg end)
  (delq nil
        (mapcar
         (lambda (ov)
           (and (string= (overlay-get ov 'before-string) "|")
                ov))
         (overlays-in beg end))))

(defun indent-guide-show (beg end)
  (flet ((indent-list ()
                      (let ((lst nil))
                        (goto-char end)
                        (while (< beg (point))
                          (back-to-indentation)
                          (setq lst (cons (current-column) lst))
                          (forward-line -1))
                        (back-to-indentation)
                        (setq lst (cons (current-column) lst))))
         (make-line (col lst)
                    (when (and lst (< col (car lst)))
                      (vertical-motion (cons col 1))
                      (let ((ovs (delq nil
                                       (mapcar
                                        (lambda (ov)
                                          (and (string= (overlay-get ov 'before-string) "|")
                                               ov))
                                        (overlays-at (1- (point))))))
                            ov)
                        (when ovs
                          (setq ov (car ovs))
                          (move-overlay ov (overlay-start ov) (1+ (overlay-end ov))))
                        (setq ov (make-overlay (point) (1+ (point))))
                        (overlay-put ov 'invisible t)
                        (overlay-put ov 'before-string
                                     (propertize "|" 'face 'indent-guide-face)))
                      (make-line col (cdr lst)))))
    (unless (indent-guide-overlays beg end)
      (save-excursion
        (let ((lst (indent-list)))
          (dotimes (elem (length lst))
            (goto-char beg)
            (when (> elem 0) (vertical-motion elem))
            (make-line (nth elem lst) (nthcdr (1+ elem) lst))))))))

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
  (unless (active-minibuffer-window)
   (save-excursion
     (ignore-errors (forward-char))        ; *FIXME*
     (let* ((beg (indent-guide-beginning-of-defun))
            (end (indent-guide-end-of-defun)))
       (indent-guide-show beg end)))))

(defun indent-guide-pre-command ()
  (save-excursion
    (ignore-errors (forward-char))        ; *FIXME*
    (let* ((beg (indent-guide-beginning-of-defun))
           (end (indent-guide-end-of-defun)))
      (indent-guide-remove beg end))))

(add-hook 'pre-command-hook 'indent-guide-pre-command)

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
