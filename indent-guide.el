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
;; URL: http://zk-phi.github.io/
;; Version: 3.0

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
;; 2.4.0 add option "indent-guide-char-top" and "-bottom"
;; 3.0.0 single O(V) forward scan for all guides in recursive mode

;;; Code:

(require 'cl-lib)

(defconst indent-guide-version "3.0.0")

;; * customs

(defgroup indent-guide nil
  "Show vertical lines to guide indentation."
  :group 'environment)

(defcustom indent-guide-char "|"
  "Character used for the guide line."
  :type 'string
  :group 'indent-guide)

(defcustom indent-guide-char-top nil
  "when non-nil, character used for the top of the guide line."
  :type 'string
  :group 'indent-guide)

(defcustom indent-guide-char-bottom nil
  "when non-nil, character used for the bottom of the guide line."
  :type 'string
  :group 'indent-guide)

(defcustom indent-guide-inhibit-modes
  '(tabulated-list-mode
    special-mode
    dired-mode
    eww-mode
    eshell-mode
    helm-major-mode
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

(defun indent-guide--indentation-candidates (level)
  "*Internal function for `indent-guide--beginning-of-level'."
  (cond ((<= level 0)
         (list ""))
        ((>= level tab-width)
         (let ((ntabs (/ level tab-width))
               (nspaces (mod level tab-width)))
           (cons (concat (make-string ntabs ?\t)
                         (make-string nspaces ?\s))
                 (cons (make-string level ?\s)
                       (indent-guide--indentation-candidates (1- level))))))
        (t
         (cons (make-string level ?\s)
               (indent-guide--indentation-candidates (1- level))))))

;; Note(vmargb): `indent-guide--beginning-of-level' is called repeatedly
;; even within the same indentation level when the cursor is moved around
;; so we cache and reuse it until the user changes to another indent level
(defvar-local indent-guide--regex-cache nil
"Stores the last computed regex with the inputs used to build it.
Format: ((BASE-LEVEL . TAB-WIDTH) . REGEX-STRING).")

(defun indent-guide--beginning-of-level ()
  "Move to the beginning of current indentation level and return
the point.  When no such points are found, just return nil."
  (back-to-indentation)
  (let* ((base-level (if (not (eolp))
                         (current-column)
                       (max (save-excursion
                              (skip-chars-forward " \t\n")
                              (current-column))
                            (save-excursion
                              (skip-chars-backward " \t\n")
                              (back-to-indentation)
                              (current-column)))))
         (cache-key (cons base-level tab-width)) ; key: indent depth & tab width
         ;; check if current inputs match regex-cache
         (regex (if (equal (car indent-guide--regex-cache) cache-key)
                    (cdr indent-guide--regex-cache) ; reuse regex string
                  ; recompute regex
                  (let ((candidates (indent-guide--indentation-candidates
                                     (1- base-level))))
                    (setq indent-guide--regex-cache
                          (cons cache-key
                                (concat "^"
                                        (regexp-opt candidates t)
                                        "[^ \t\n]")))
                    (cdr indent-guide--regex-cache)))))
    (unless (zerop base-level)
      (and (search-backward-regexp regex nil t)
           (goto-char (match-end 1))))))

;;; NOTE(arka): custom fn for decorated guide line
(defun indent-guide--choose-char (line line-start line-end)
  "Return the appropriate guide character for LINE."
  (if (= line-start line-end)
      indent-guide-char
    (cond
     ((= line line-start) (or indent-guide-char-top indent-guide-char))
     ((= line line-end) (or indent-guide-char-bottom indent-guide-char))
     (t indent-guide-char)))
  )

;;; single-pass recursive mode

;; NOTE(vmargb): In recursive mode the old code called indent-guide-show
;; recursively, and each call ran indent-guide--beginning-of-level
;; (a backward regex search) plus a forward scan to find the block end
;; which is O(N * B) where B is the backward search distance.
;; This function replaces all of that with a single O(V)
;; forward scan over the V visible lines, using a stack to track open
;; blocks. No regex needed or recursion needed anymore!
(defun indent-guide--compute-guides (win-start win-end)
  "Scan from WIN-START to WIN-END and return all indent guides.
list of (COL LINE-START LINE-END) tuples by maintaining a
stack of open blocks.  Each entry is (GUIDE-COL . START-LINE)."
  (let ((guides nil)
        (stack  nil)
        (line   (line-number-at-pos win-start))
        (end    (line-number-at-pos win-end))
        (prev   0))
    (save-excursion
      (goto-char win-start)
      (setq prev (if (looking-at "[ \t]*$") 0 (current-indentation)))
      ; (> prev indent-guide-threshold) so far-left column is included in stack
      (when (> prev indent-guide-threshold)
        (push (cons prev line) stack))
      (while (<= line end)
        (let ((ind (if (looking-at "[ \t]*$") prev (current-indentation))))
          ;; close guides at or above current indentation
          (while (and stack (>= (caar stack) ind))
            (when (<= (cdar stack) (1- line))
              (push (list (caar stack) (cdar stack) (1- line)) guides))
            (pop stack))
          ;; open a new guide on increased indentation
          (when (and (> ind indent-guide-threshold)
                     (or (null stack) (> ind (caar stack))))
            (push (cons ind (1+ line)) stack))
          (setq prev ind))
        (forward-line 1)
        (setq line (1+ line)))
      ;; flush guides still open at viewport bottom
      (dolist (e stack)
        (when (<= (cdr e) end)
          (push (list (car e) (cdr e) end) guides))))
    guides))

;; * generate guides

;;; NOTE(arka): extra `line-start` and `line-end` are parameters added for decorated guide line
(defun indent-guide--make-overlay (line col line-start line-end)
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
                           (overlays-at (point))))
                 ;; we already have an overlay here => append to the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'before-string)))
                                (concat str
                                        (make-string (- diff (length str)) ?\s)
                                        ;;; NOTE(arka): automatic indentaiton guide character selection
                                        ;;; based on line number count.
                                        (propertize (indent-guide--choose-char line line-start line-end)
                                                    'face 'indent-guide-face)))
                       prop   'before-string)
               (setq string (concat (make-string diff ?\s)
                                    (propertize (indent-guide--choose-char line line-start line-end)
                                                'face 'indent-guide-face))
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
                                    (propertize (indent-guide--choose-char line line-start line-end)
                                                'face 'indent-guide-face)
                                    (make-string (1- (- diff)) ?\s))
                     prop   'display
                     ov     (make-overlay (point) (1- (point))))))
            ((looking-at "\t")          ; okay but looking at tab
             ;;    <-tab-width->
             ;; [|]
             (setq string (concat (propertize (indent-guide--choose-char line line-start line-end)
                                              'face 'indent-guide-face)
                                  (make-string (1- tab-width) ?\s))
                   prop   'display
                   ov     (make-overlay (point) (1+ (point)))))
            (t                          ; okay and looking at a space
             (setq string (propertize (indent-guide--choose-char line line-start line-end)
                                      'face 'indent-guide-face)
                   prop   'display
                   ov     (make-overlay (point) (1+ (point))))))
      (when ov
        (overlay-put ov 'category 'indent-guide)
        (overlay-put ov prop string)))))

;; NOTE(vmargb): indent-guide-show now branches on indent-guide-recursive.
;; In recursive mode it uses indent-guide--compute-guides (single forward
;; scan of the viewport) for all visible blocks, but then additionally draws
;; the cursor's own block ancestry so outer guides stay visible when the
;; function header *scrolls off-screen*.
(defun indent-guide-show ()
  "Show indent-guide lines, for both recursive and non-recursive."
  (interactive)
  ;;; NOTE(arka): redraw only when needed
  (unless (active-minibuffer-window)
    (let ((win-start (window-start))
          (win-end (window-end nil t))
          line-col line-start line-end)
      ;;; only clear overlays in the visible viewport
      (indent-guide-remove win-start win-end)

      (if indent-guide-recursive
          (progn
            ;; single-pass, all guides visible in the viewport
            (let ((guides (indent-guide--compute-guides win-start win-end)))
              (dolist (g guides)
                (let ((col    (nth 0 g))
                      (lstart (nth 1 g))
                      (lend   (nth 2 g)))
                  (when (> col indent-guide-threshold)
                    (dotimes (i (- (1+ lend) lstart))
                      (let ((ln (+ lstart i)))
                        ;; skip blank lines
                        (unless (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- ln))
                                  (looking-at "[ \t]*$"))
                          (indent-guide--make-overlay ln col lstart lend)))))))))

        ;; original non-recursive path (unchanged)
        ;; decide line-col, line-start
        (save-excursion
          (indent-guide--beginning-of-level)
          (setq line-col (current-column)
                line-start (max (1+ (line-number-at-pos))
                                (line-number-at-pos win-start))))
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
            (let ((ln (+ line-start tmp)))
              ;; skip blank lines (same reason as recursive path)
              (unless (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- ln))
                        (looking-at "[ \t]*$"))
                (indent-guide--make-overlay ln line-col line-start line-end))))
          (remove-overlays (point) (point) 'category 'indent-guide))))))

;; use built-in `remove-overlays'
(defun indent-guide-remove (&optional beg end)
  "Remove indent-guide overlays between BEG and END.
Defaults to the whole buffer if not provided."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'category 'indent-guide))

;; * minor-mode

;; use named function to prevent a lambda closure being
;; allocated repeatedly on every debounce
(defun indent-guide--run-timer ()
  (indent-guide-show)
  (setq indent-guide--timer-object nil))

;;; NOTE(arka): root cause of flickering effect. we don't actually need
;;; pre-hook to redraw guides on each command.
;; (defun indent-guide-pre-command-hook ()
;;   ;; some commands' behavior may affected by indent-guide overlays, so
;;   ;; remove all overlays in pre-command-hook.
;;   (indent-guide-remove))

;; Note(vmargb): the timer now behaves like a proper `debounce'
;; every new command cancels the old idle timer and schedules a new one
;; so `indent-guide-show' only runs after the user has paused, not after
;; the first command in a burst.
;; Used by both hooks: `post-command-hook' & `window-scroll-functions'.
(defun indent-guide--request-show (&rest _)
  (if (null indent-guide-delay)
      (indent-guide-show) ; no delay, show immediately
    (when indent-guide--timer-object ; is delay, so cancel/debounce
      (cancel-timer indent-guide--timer-object))
    (setq indent-guide--timer-object ; schedule new timer
          (run-with-idle-timer indent-guide-delay nil
                               #'indent-guide--run-timer))))

;;;###autoload
(define-minor-mode indent-guide-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indent-guide-mode
      (progn
        ;;; NOTE(arka): only use post-hook. pre-hook is now depricated
        (add-hook 'post-command-hook 'indent-guide--request-show nil t)
        (add-hook 'window-scroll-functions 'indent-guide--request-show nil t))
    (remove-hook 'post-command-hook 'indent-guide--request-show t)
    (remove-hook 'window-scroll-functions 'indent-guide--request-show t)))

;;;###autoload
(define-globalized-minor-mode indent-guide-global-mode
  indent-guide-mode
  (lambda ()
    (unless (cl-some 'derived-mode-p indent-guide-inhibit-modes)
      (indent-guide-mode 1))))

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
