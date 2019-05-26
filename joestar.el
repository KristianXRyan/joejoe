;;; package --- summary Wordstar/joe compatibility mode.

;;; Copyright (C) Ryan Jeffrey 2019

;;; Author: Ryan Jeffrey <pwishie@gmail.com>
;;; Created: 2019-05-12
;;; Keywords: joe wordstar emulation editor
;;; Version: 0.2
;;; Package-Requires: ((emacs "24.2") (undo-tree "0.8.5"))
;;; URL: https://github.com/Ma11ock/joestar

;;; License:

;; This file is part of Joestar.
;;
;; Joestar is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Joestar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Joestar.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Joestar is a minor mode for Emacs that seeks to emulate Joe's Own Editor, a wordstar-like terminal text editor.

;;; Code:

;;; TODO: Confine buffers to certain windows; better relative line number support with goto-line
;;; TODO: remake with lambdas

;;; set mark variables
(defvar joe-mark-0 nil "Mark 0.")
(defvar joe-mark-1 nil "Mark 1.")
(defvar joe-mark-2 nil "Mark 2.")
(defvar joe-mark-3 nil "Mark 3.")
(defvar joe-mark-4 nil "Mark 4.")
(defvar joe-mark-5 nil "Mark 5.")
(defvar joe-mark-6 nil "Mark 6.")
(defvar joe-mark-7 nil "Mark 7.")
(defvar joe-mark-8 nil "Mark 8.")
(defvar joe-mark-9 nil "Mark 9.")

(make-variable-buffer-local 'joe-mark-0)
(make-variable-buffer-local 'joe-mark-1)
(make-variable-buffer-local 'joe-mark-2)
(make-variable-buffer-local 'joe-mark-3)
(make-variable-buffer-local 'joe-mark-4)
(make-variable-buffer-local 'joe-mark-5)
(make-variable-buffer-local 'joe-mark-6)
(make-variable-buffer-local 'joe-mark-7)
(make-variable-buffer-local 'joe-mark-8)
(make-variable-buffer-local 'joe-mark-9)

;; TODO function does not work
(defun joe/setmark (sid)
  "Set the mark SID manually."
  (interactive "sSet Mark (0-9): ")
  (let (id (string-to-number sid))
    (cond ((eq id 0) (joe/set-mark-0))
          ((eq id 1) (joe/set-mark-1)))))

(defun joe/set-mark-0 ()
  "Set the 0th mark."
  (interactive)
  (setq joe-mark-0 (point-marker))
  (message "Mark 0 set."))

(defun joe/set-mark-1 ()
  "Set the 1st mark."
  (interactive)
  (setq joe-mark-1 (point-marker))
  (message "Mark 1 set."))

;;; setting joestar's wordstar-like keybindings
(defvar  joestar-mode-map
  (let ((joe-map (make-sparse-keymap)))
    ;; region
    (define-key joe-map (kbd "<C-right>") '(lambda ()
                                             "Control selection to right char."
                                             (interactive)
                                             (setq this-command-keys-shift-translated t)
                                             (call-interactively 'forward-char)))
    (define-key joe-map (kbd "<C-left>") '(lambda ()
                                            "Control selection to left char."
                                            (interactive)
                                            (setq this-command-keys-shift-translated t)
                                            (call-interactively 'backward-char)))
    (define-key joe-map (kbd "<C-up>") '(lambda ()
                                          "Control selection up one line."
                                          (interactive)
                                          (setq this-command-keys-shift-translated t)
                                          (call-interactively 'previous-line)))
    (define-key joe-map (kbd "<C-down>") '(lambda ()
                                            "Control selection down one line."
                                            (interactive)
                                            (setq this-command-keys-shift-translated t)
                                            (call-interactively 'next-line)))
    (define-key joe-map (kbd "C-k C-y") 'delete-region)
    (define-key joe-map (kbd "C-k y") (kbd "C-k C-y"))
    (define-key joe-map (kbd "C-k C-m") '(lambda ()
                                           "Move the current block to point."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k m") (kbd "C-k C-m"))
    (define-key joe-map (kbd "C-k C-c") 'copy-region-as-kill) ;; TODO test
    (define-key joe-map (kbd "C-k c") (kbd "C-k C-c"))
    (define-key joe-map (kbd "C-k C-/") '(lambda ()
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k /") (kbd "C-k C-/"))
    (define-key joe-map (kbd "C-k C-w") '(lambda (file-path)
                                           "Writes the region to FILE-PATH."
                                           (write-region (region-beginning) (region-end) file-path))) ; TODO test
    (define-key joe-map (kbd "C-k w") (kbd "C-k C-w"))
    
    ;; goto
    (define-key joe-map (kbd "C-z") 'backward-word)
    (define-key joe-map (kbd "C-x") 'forward-word)
    (define-key joe-map (kbd "C-k C-u") 'beginning-of-buffer)
    (define-key joe-map (kbd "C-k u") (kbd "C-k C-u"))
    (define-key joe-map (kbd "C-k C-v") 'end-of-buffer)
    (define-key joe-map (kbd "C-k v") (kbd "C-k C-v"))
    (define-key joe-map (kbd "C-u") 'scroll-down)
    (define-key joe-map (kbd "C-v") 'scroll-up)
    (define-key joe-map (kbd "C-e") 'end-of-line)
    (define-key joe-map (kbd "C-a") 'beginning-of-line-text)
    (define-key joe-map (kbd "C-k C-l") '(lambda (num)
                                           "Goto absolute line number NUM."
                                           (interactive "sGo to line: ")
                                           (forward-line (* -1 (count-lines 1 (point))))
                                           (forward-line (1- (string-to-number num)))))
    (define-key joe-map (kbd "C-k l") (kbd "C-k C-l"))
    (define-key joe-map (kbd "C-g") '(lambda ()
                                       (interactive))) ; TODO

    ;; misc
    (define-key joe-map (kbd "C-k C-j") '(lambda ()
                                           "Turns a really long line into a paragraph of multiple lines."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k j") (kbd "C-k C-j"))
    (define-key joe-map (kbd "C-k C-a") 'center-line)
    (define-key joe-map (kbd "C-k a") (kbd "C-k C-a"))
    (define-key joe-map (kbd "<C-k C-space>") '(lambda ()
                                                 "Print current buffer information to the minibuffer."
                                                 (interactive))) ; TODO
    (define-key joe-map (kbd "<C-k space>") (kbd "<C-k C-space>"))

    ;; spell
    (define-key joe-map (kbd "<escape> n") 'flyspell-word)
    (define-key joe-map (kbd "<escape> l") 'flyspell-buffer)

    ;; delete
    (define-key joe-map (kbd "<backspace>") '(lambda ()
                                               "Redefine the what the backspace key does to mimic vim."
                                               (interactive)
                                               (cond ((bolp)
                                                      (delete-char -1)
                                                      (indent-according-to-mode)
                                                      (when (looking-at "\\([ \t]+\\)[^ \t]")
                                                        (delete-region (point) (match-end 1))))
                                                     ((<= (point) (save-excursion (back-to-indentation) (point)))
                                                      (let ((backward-delete-char-untabify-method 'hungry))
                                                        (call-interactively 'backward-delete-char-untabify)
                                                        (delete-char -1))
                                                      (indent-according-to-mode))
                                                     (t
                                                      (let ((backward-delete-char-untabify-method 'hungry))
                                                        (call-interactively 'backward-delete-char-untabify))))))
    (define-key joe-map (kbd "C-y") 'kill-whole-line)
    (define-key joe-map (kbd "C-w") 'kill-word)
    (define-key joe-map (kbd "C-o") '(lambda ()
                                       "Kill the word before point."
                                       "Kill the word to the left of the cursor."
                                       (interactive)
                                       (call-interactively 'backward-word)
                                       (call-interactively 'kill-word))) ;; TODO, Test
    (define-key joe-map (kbd "C-j") 'kill-line)
    (define-key joe-map (kbd "<escape> o") '(lambda ()
                                         "Kill to the beginning of the line before point."
                                         (kill-line 0)))
    (define-key joe-map (kbd "C-^") '(lambda ()
                                       "Calls undo-tree redo."
                                       (interactive)
                                       (call-interactively 'undo-tree-redo)))
    (define-key joe-map (kbd "C-_") '(lambda ()
                                       "Calls undo-tree undo."
                                       (interactive)
                                       (call-interactively 'undo-tree-undo)))

    ;; exit
    (define-key joe-map (kbd "C-k C-x") 'save-buffers-kill-emacs)
    (define-key joe-map (kbd "C-k x") (kbd "C-k C-x"))
    (define-key joe-map (kbd "C-c") '(lambda ()
                                       "Abort."
                                       (interactive)))
    (define-key joe-map (kbd "C-k C-q") 'kill-emacs)
    (define-key joe-map (kbd "C-k q") (kbd "C-k C-q"))

    ;; file
    (define-key joe-map (kbd "C-k C-e") 'find-file)
    (define-key joe-map (kbd "C-k e") 'find-file)
    (define-key joe-map (kbd "C-k C-r") '(lambda ()
                                           "Prompt the minibuffer to save the current buffer as FILE-PATH."
                                           (interactive "FName of file to insert: ")
                                           (insert-file-contents file-path)))
    (define-key joe-map (kbd "C-k r") (kbd "C-k C-r"))
    (define-key joe-map (kbd "C-k C-d") '(lambda (file-path)
                                           "Prompt the minibuffer to save the current buffer as FILE-PATH."
                                           (interactive "FName of file to save: ")
                                           (save-buffer file-path)
                                           (message "%s saved." file-path)))
    (define-key joe-map (kbd "C-k d") (kbd "C-k C-d"))
    (define-key joe-map (kbd "C-k C-`") '(lambda (file-path)
                                           "Prompt the minibuffer to save the current buffer as FILE-PATH."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k `") (kbd "C-k C-`"))
    (define-key joe-map (kbd "C-c") '(lambda ()
                                       "Delete current window, exit Emacs if only one window."
                                       (interactive)
                                       (if (= 1 (length (window-list)))
                                           (if (buffer-modified-p)
                                               (if (y-or-n-p "Lose changes to this file? ")
                                                   (kill-emacs)
                                                 nil)
                                             (kill-emacs))
                                         (delete-window)))) ; TODO

    
    ;; search
    (define-key joe-map (kbd "C-k C-f") '(lambda ()
                                           "Joe-style find function."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k C-f") (kbd "C-k C-f"))

    ;; help TODO
    (define-key joe-map (kbd "C-k C-h") '(lambda ()
                                           "Set up or turn off the help buffer."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k h") (kbd "C-k C-h"))

    ;; macros TODO
    (define-key joe-map (kbd "C-k C-[") '(lambda (num)
                                           "Set a macro with id NUM."
                                           (interactive "%s"))) ; TODO
    (define-key joe-map (kbd "C-k [") (kbd "C-k C-["))
    (define-key joe-map (kbd "C-k C-]") 'end-kbd-macro) ; TODO ?
    (define-key joe-map (kbd "C-k ]") (kbd "C-k C-]"))
    (define-key joe-map (kbd "C-k C-\\") '(lambda() (interactive))) ; TODO ?
    (define-key joe-map (kbd "C-k \\") (kbd "C-k C-\\"))
    (define-key joe-map (kbd "<escape> D") '(lambda() (interactive))) ; TODO ?

    ;; scroll
    (define-key joe-map (kbd "<escape> w") 'scroll-down-line)
    (define-key joe-map (kbd "<escape> z") 'scroll-up-line)
    (define-key joe-map (kbd "<escape> <") 'scroll-left) ; TODO test
    (define-key joe-map (kbd "<escape> >") 'scroll-right) ; TODO test

    ;; insert
    (define-key joe-map (kbd "C-]") '(lambda ()
                                       "Insert a newline char."
                                       (interactive)
                                       (insert "\n"))) ; TODO test
    (define-key joe-map (kbd "C-@") '(lambda ()
                                       "Insert a space char."
                                       (interactive)
                                       (insert " ")))
    (define-key joe-map (kbd "C-q") '(lambda ()(interactive))) ; TODO
    (define-key joe-map (kbd "<escape> y") '(lambda () (interactive))) ; TODO

    ;; window
    (define-key joe-map (kbd "C-k C-o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k o") (kbd "C-k C-o"))
    (define-key joe-map (kbd "C-k C-g") 'enlarge-window)
    (define-key joe-map (kbd "C-k g") (kbd "C-k C-g"))
    (define-key joe-map (kbd "C-k C-t") 'shrink-window)
    (define-key joe-map (kbd "C-k t") (kbd "C-k C-t"))
    (define-key joe-map (kbd "C-k C-n") '(lambda ()
                                           "Move to the next window."
                                           (interactive)
                                           (other-window 1)))
    (define-key joe-map (kbd "C-k n") (kbd "C-k C-n"))
    (define-key joe-map (kbd "C-k C-p") '(lambda ()
                                           "Move to the previous window."
                                           (interactive)
                                           (other-window -1)))
    (define-key joe-map (kbd "C-k p") (kbd "C-k C-p"))
    (define-key joe-map (kbd "C-k C-i") '(lambda ()
                                           "Show one / All."
                                           (interactive))) ; TODO
    (define-key joe-map (kbd "C-k i") (kbd "C-k C-i"))
    (define-key joe-map (kbd "M-+")  'text-scale-increase)
    (define-key joe-map (kbd "M--")  'text-scale-decrease)
    

    ;; shell TODO
    ;; in joe, the cursor does not change when the comand is appended.
    (define-key joe-map (kbd "<escape> !") '(lambda (com)
                                              "Appends the output of shell command COM to current buffer."
                                              (interactive "Program to run: ")
                                              (append-to-buffer (shell-command-to-string com)))) ; TODO test,
    (define-key joe-map (kbd "C-k C-z") 'suspend-emacs)
    (define-key joe-map (kbd "C-k z") (kbd "C-k C-z"))
    joe-map)
  "The joestar-mode keymaps.")

;;; defining the minor modes

(define-minor-mode joestar-mode
  "Joe's Own Editor emulation mode."
  :init-value nil
  :lighter " joe"
  nil
  
  ;; setting default Emacs behaviour
  (setq scroll-margin 1
        scroll-conservatively 0
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01)

  ;; keep cursor position while scrolling
  (setq scroll-preserve-screen-position 'always)
  (setq scroll-error-top-bottom nil)
  (defvar undo-tree-map
    (let ((map (make-sparse-keymap)))
      ;; remap `undo' and `undo-only' to `undo-tree-undo'
      (define-key map [remap undo] 'undo-tree-undo)
      (define-key map [remap undo-only] 'undo-tree-undo)
      ;; bind standard undo bindings (since these match redo counterparts)
      (define-key map (kbd "C-/") 'undo-tree-undo)
      (define-key map "\C-_" 'undo-tree-undo)
      ;; redo doesn't exist normally, so define our own keybindings
      (define-key map (kbd "C-?") 'undo-tree-redo)
      (define-key map (kbd "M-_") 'undo-tree-redo)
      ;; just in case something has defined `redo'...
      (define-key map [remap redo] 'undo-tree-redo)
      ;; we use "C-x u" for the undo-tree visualizer
      (define-key map (kbd "s-x u") 'undo-tree-visualize)
      ;; bind register commands
      (define-key map (kbd "s-x r u") 'undo-tree-save-state-to-register)
      (define-key map (kbd "s-x r U") 'undo-tree-restore-state-from-register)
      ;; set keymap
      (setq undo-tree-map map)))
  (undo-tree-mode t))

(define-globalized-minor-mode global-joestar-mode joestar-mode
  (lambda () (joestar-mode 1)))

(provide 'joestar)
;;; joestar.el ends here
