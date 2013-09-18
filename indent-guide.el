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
;; Version: 2.1.3

;;; Commentary:

;; Require this script
;;
;;   (require 'indent-guide)
;;
;; and call command "M-x indent-guide-mode".

;; If you want to enable indent-guide-mode automatically,
;; call "indent-guide-global-mode" function.
;;
;;   (indent-guide-global-mode)

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
;; 2.0.3 added indent-guide-global-mode
;; 2.1.0 now lines are not drawn over the cursor
;; 2.1.1 work better with blank lines
;; 2.1.2 fixed bug in empty files
;; 2.1.3 better bob and eob handling

;;; Code:

(defconst indent-guide-version "2.1.3")

;; * customs

(defgroup indent-guide nil
  "show vertical lines to guide indentation"
  :group 'emacs)

(defcustom indent-guide-char "|"
  "character used as vertical line"
  :group 'indent-guide)

;; * minor-mode

(define-minor-mode indent-guide-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indent-guide-mode
      (progn
        (add-hook 'pre-command-hook 'indent-guide-remove nil t)
        (add-hook 'post-command-hook 'indent-guide-show nil t))
    (remove-hook 'pre-command-hook 'indent-guide-remove t)
    (remove-hook 'post-command-hook 'indent-guide-show t)))

(define-globalized-minor-mode indent-guide-global-mode
  indent-guide-mode
  (lambda () (indent-guide-mode 1)))

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

(defun indent-guide--beginning-of-level (&optional origin)
  ;; origin <- indent column of current line
  (unless origin
    (back-to-indentation)
    (if (not (eolp))
        (setq origin (current-column))
      (let ((forward (save-excursion
                       (while (and (forward-line 1)
                                   (not (eobp))
                                   (progn (back-to-indentation) t)
                                   (eolp)))
                       (if (eobp) 0 (current-column))))
            (backward (save-excursion
                        (while (and (zerop (forward-line -1))
                                    (progn (back-to-indentation) t)
                                    (eolp)))
                        (if (bobp) 0 (current-column)))))
        (setq origin (max forward backward)))))
  (cond ((zerop origin)
         (point))
        ((= (forward-line -1) -1)
         nil)
        ((progn
           (back-to-indentation)
           (and (not (eolp))
                (< (current-column) origin)))
         (point))
        (t
         (indent-guide--beginning-of-level origin))))

;; * generate guides

(defun indent-guide--make-overlay (line col)
  "draw line at (line, col)"
  (let ((original-pos (point))
        diff string ov)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      ;; calculate difference from the actual col
      (setq diff (- (current-column) col))
      ;; make overlay or not
      (cond ((eolp)                     ; blank line (with no or less indent)
             (setq string (concat (make-string (- diff) ?\s)
                                  indent-guide-char)
                   ov     (and (not (= (point) original-pos))
                               (make-overlay (point) (point)))))
            ((not (zerop diff))         ; looking back tab
             (setq string (concat (make-string (- tab-width diff) ?\s)
                                  indent-guide-char
                                  (make-string (1- diff) ?\s))
                   ov     (and (not (= (point) (1- original-pos)))
                               (make-overlay (point) (1- (point))))))
            ((looking-at "\t")          ; looking at tab
             (setq string (concat indent-guide-char
                                  (make-string (1- tab-width) ?\s))
                   ov     (and (not (= (point) original-pos))
                               (make-overlay (point) (1+ (point))))))
            (t                          ; no problem
             (setq string indent-guide-char
                   ov     (and (not (= (point) original-pos))
                               (make-overlay (point) (1+ (point)))))))
      (when ov
        (overlay-put ov 'invisible t)
        (overlay-put ov 'category 'indent-guide)
        (overlay-put ov 'before-string
                     (propertize string 'face 'indent-guide-face))))))

(defun indent-guide-show ()
  (unless (or (indent-guide--active-overlays)
              (active-minibuffer-window))
    (let ((win-start (window-start))
          (win-end (window-end))
          line-col line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (if (not (indent-guide--beginning-of-level))
            (setq line-col 0
                  line-start 1)
          (setq line-col (current-column)
                line-start (max (1+ (line-number-at-pos))
                                (line-number-at-pos win-start)))))
      ;; decide line-end
      (save-excursion
        (while (and (progn (back-to-indentation)
                           (or (< line-col (current-column)) (eolp)))
                    (forward-line 1)
                    (not (eobp))
                    (<= (point) win-end)))
        (if (>= line-col (current-column))
            (forward-line -1))
        (setq line-end (line-number-at-pos)))
      ;; draw line
      (dotimes (tmp (- (1+ line-end) line-start))
        (indent-guide--make-overlay (+ line-start tmp) line-col)))))

(defun indent-guide-remove ()
  (dolist (ov (indent-guide--active-overlays))
    (delete-overlay ov)))

;; * workaround (FIXME)

(defadvice indent-guide-show (around indent-guide--workaround activate)
  (condition-case err
      ad-do-it
    (error (progn (message "an error occurred in indent-guide-show")
                  (sit-for 1)))))

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
