;;; indent-guide.el --- show vertical lines to guide indentation

;; Copyright (C) 2013- zk_phi

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
;; Version: 2.3.1

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
;; 2.1.4 use "display" property instead of "before-string"
;;       (now works better with hl-line and linum)
;; 2.1.5 add "indent-guide-inhibit-modes"
;; 2.1.6 add option "indent-guide-recursive"
;; 2.2.0 add option "indent-guide-threshold"
;; 2.3.0 use regexp search to find the beginning of level
;; 2.3.1 add option "indent-guide-lispy-modes"

;;; Code:

(require 'cl-lib)

(defconst indent-guide-version "2.3.1")

;; * customs

(defgroup indent-guide nil
  "Show vertical lines to guide indentation."
  :group 'environment)

(defcustom indent-guide-char "|"
  "Character used as vertical line."
  :type 'string
  :group 'indent-guide)

(defcustom indent-guide-inhibit-modes
  '(tabulated-list-mode
    special-mode
    dired-mode
    eww-mode
    eshell-mode
    Custom-mode)
  "List of major-modes in which indent-guide should be turned off."
  :type '(repeat symbol)
  :group 'indent-guide)

(defcustom indent-guide-recursive nil
  "When non-nil, draw multiple guide lines recursively."
  :type 'boolean
  :group 'indent-guide)

(defcustom indent-guide-delay nil
  "When a positive number, rendering guide lines is delayed DELAY
  seconds."
  :type 'number
  :group 'indent-guide)

(defcustom indent-guide-threshold -1
  "Guide lines are drawn only when the column number is over this
  value."
  :type 'number
  :group 'indent-guide)

(defcustom indent-guide-lispy-modes
  '(lisp-mode emacs-lisp-mode scheme-mode
              lisp-interaction-mode gauche-mode scheme-mode
              clojure-mode racket-mode egison-mode)
  "List of lisp-like language modes, in which the last brace of
blocks are NOT placed at beginning of line."
  :type '(repeat symbol)
  :group 'indent-guide)

(defface indent-guide-face '((t (:foreground "#535353" :slant normal)))
  "Face used to indent guide lines."
  :group 'indent-guide)

;; * variables

(defvar indent-guide--timer-object nil)

;; * utilities

(defun indent-guide--active-overlays ()
  "Return the list of all overlays created by indent-guide."
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-guide) ov))
         (overlays-in (point-min) (point-max)))))

(defun indent-guide--indentation-candidates (level)
  "*Internal function for `indent-guide--beginning-of-level'."
  (cond ((<= level 0)
         (list ""))
        ((>= level tab-width)
         (cons (concat "\t" (make-string (- level tab-width) ?\s))
               (cons (make-string level ?\s)
                     (indent-guide--indentation-candidates (1- level)))))
        (t
         (cons (make-string level ?\s)
               (indent-guide--indentation-candidates (1- level))))))

(defun indent-guide--beginning-of-level ()
  "Move to the beginning of current indentation level and return
the point. When no such points are found, just return nil."
  (back-to-indentation)
  (let* ((base-level (if (not (eolp))
                         (current-column)
                       (max (save-excursion
                              (skip-chars-forward "\s\t\n")
                              (current-column))
                            (save-excursion
                              (skip-chars-backward "\s\t\n")
                              (back-to-indentation)
                              (current-column)))))
         (candidates (indent-guide--indentation-candidates (1- base-level)))
         (regex (concat "^" (regexp-opt candidates t) "[^\s\t\n]")))
    (unless (zerop base-level)
      (and (search-backward-regexp regex nil t)
           (goto-char (match-end 1))))))

;; * generate guides

(defun indent-guide--make-overlay (line col)
  "draw line at (line, col)"
  (let (diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      ;; calculate difference from the actual col
      (setq diff (- col (current-column)))
      ;; make overlay or not
      (cond ((and (eolp) (<= 0 diff))   ; the line is too short
             ;; <-line-width->  <-diff->
             ;;               []        |
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-guide)
                               ov))
                           (overlays-in (point) (point))))
                 ;; we already have an overlay here => append to the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'before-string)))
                                (concat str
                                        (make-string (- diff (length str)) ?\s)
                                        (propertize indent-guide-char 'face 'indent-guide-face)))
                       prop   'before-string)
               (setq string (concat (make-string diff ?\s)
                                    (propertize indent-guide-char 'face 'indent-guide-face))
                     prop   'before-string
                     ov     (make-overlay (point) (point)))))
            ((< diff 0)                 ; the column is inside a tab
             ;;  <---tab-width-->
             ;;      <-(- diff)->
             ;;     |            []
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-guide)
                               ov))
                           (overlays-in (1- (point)) (point))))
                 ;; we already have an overlay here => modify the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'display)))
                                (aset str (+ 1 tab-width diff) ?|)
                                str)
                       prop   'display)
               (setq string (concat (make-string (+ tab-width diff) ?\s)
                                    (propertize indent-guide-char 'face 'indent-guide-face)
                                    (make-string (1- (- diff)) ?\s))
                     prop   'display
                     ov     (make-overlay (point) (1- (point))))))
            ((looking-at "\t")          ; okay but looking at tab
             ;;    <-tab-width->
             ;; [|]
             (setq string (concat (propertize indent-guide-char 'face 'indent-guide-face)
                                  (make-string (1- tab-width) ?\s))
                   prop   'display
                   ov     (make-overlay (point) (1+ (point)))))
            (t                          ; okay and looking at a space
             (setq string (propertize indent-guide-char 'face 'indent-guide-face)
                   prop   'display
                   ov     (make-overlay (point) (1+ (point))))))
      (when ov
        (overlay-put ov 'category 'indent-guide)
        (overlay-put ov prop string)))))

(defun indent-guide-show ()
  (interactive)
  (unless (or (indent-guide--active-overlays)
              (active-minibuffer-window))
    (let ((win-start (window-start))
          (win-end (window-end nil t))
          line-col line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (indent-guide--beginning-of-level)
        (setq line-col (current-column)
              line-start (max (1+ (line-number-at-pos))
                              (line-number-at-pos win-start)))
        ;; if recursive draw is enabled and (line-col > 0), recurse
        ;; into lower level.
        (when (and indent-guide-recursive (> line-col 0))
          (indent-guide-show)))
      (when (> line-col indent-guide-threshold)
        ;; decide line-end
        (save-excursion
          (while (and (progn (back-to-indentation)
                             (or (< line-col (current-column)) (eolp)))
                      (forward-line 1)
                      (not (eobp))
                      (<= (point) win-end)))
          (cond ((< line-col (current-column))
                 (setq line-end (line-number-at-pos)))
                ((not (memq major-mode indent-guide-lispy-modes))
                 (setq line-end (1- (line-number-at-pos))))
                (t
                 (skip-chars-backward "\s\t\n")
                 (setq line-end (line-number-at-pos)))))
        ;; draw line
        (dotimes (tmp (- (1+ line-end) line-start))
          (indent-guide--make-overlay (+ line-start tmp) line-col))
        (remove-overlays (point) (point) 'category 'indent-guide)))))

(defun indent-guide-remove ()
  (dolist (ov (indent-guide--active-overlays))
    (delete-overlay ov)))

;; * minor-mode

(defun indent-guide-post-command-hook ()
  (if (null indent-guide-delay)
      (indent-guide-show)
    (when (null indent-guide--timer-object)
      (setq indent-guide--timer-object
            (run-with-idle-timer indent-guide-delay nil
                                 (lambda ()
                                   (indent-guide-show)
                                   (setq indent-guide--timer-object nil)))))))

(defun indent-guide-pre-command-hook ()
  ;; some commands' behavior may affected by indent-guide overlays, so
  ;; remove all overlays in pre-command-hook.
  (indent-guide-remove))

;;;###autoload
(define-minor-mode indent-guide-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indent-guide-mode
      (progn
        (add-hook 'pre-command-hook 'indent-guide-pre-command-hook nil t)
        (add-hook 'post-command-hook 'indent-guide-post-command-hook nil t))
    (remove-hook 'pre-command-hook 'indent-guide-pre-command-hook t)
    (remove-hook 'post-command-hook 'indent-guide-post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode indent-guide-global-mode
  indent-guide-mode
  (lambda ()
    (unless (cl-some 'derived-mode-p indent-guide-inhibit-modes)
      (indent-guide-mode 1))))

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
