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

;;; TODO have a cons cell of a mark and its id?

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

(defvar joe-lastmark nil "The last mark id.")
(defvar joe-nextmark nil "The next mark id.")

(defvar joe-marklist nil "List of the currently used marks.")

(defvar joe-prev-search nil "The last searched-for item.")

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

(make-variable-buffer-local 'joe-lastmark)
(make-variable-buffer-local 'joe-nextmark)

(make-variable-buffer-local 'joe-prev-search)

;; non-interactive helper functions

; TODO lambda-ize
(defun joe-get-findstr ()
  "Get the string to search for."
  (if (null joe-prev-search)
      (read-string "Find: ")
    (read-string (format "Find [%s]: " joe-prev-search))))

(defun joe-get-find-action ()
  "Prompt the user for an action."
  (read-string "(I)gnore (R)eplace (B)ackwards Bloc(K): "))

(defun joe-todo-func ()
  "TODO func."
  (interactive))

(defun joe-ctr-selection-right ()
  "Control selection to right char."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'forward-char))
(defun joe-ctr-selection-left ()
  "Control selection to left char."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'backward-char))
(defun joe-ctr-selection-up ()
  "Control selection up one line."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'previous-line))
(defun joe-ctr-selection-down ()
  "Control selection down one line."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'next-line))

(defun joe-line (num)
  "Goto absolute line number NUM."
  (interactive "sGo to line: ")
  (forward-line (* -1 (count-lines 1 (point))))
  (forward-line (1- (string-to-number num))))

(defun joe-backspace ()
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
           (call-interactively 'backward-delete-char-untabify)))))

(defun joe-delete-word-left ()
  "Kill the word to the left of the cursor."
  (interactive)
  (call-interactively 'backward-word)
  (call-interactively 'kill-word))

(defun joe-redo ()
  "Call undo-tree redo."
  (interactive)
  (call-interactively
   'undo-tree-redo))

(defun joe-undo ()
  "Call undo-tree undo."
  (interactive)
  (call-interactively 'undo-tree-undo))

; TODO
(defun joe-reloadall ()
  "Revert all unmodified buffers."
  (interactive))

(defun joe-scratch (name)
  "Push a scratch buffer NAME into current window."
  (interactive "sName of buffer to edit: ")
  (generate-new-buffer (generate-new-buffer-name name))
  (switch-to-buffer name))

(defun joe-insf (file-path)
  "Prompt the minibuffer to save the current buffer as FILE-PATH."
  (interactive "FName of file to insert: ")
  (insert-file-contents file-path))

(defun joe-save (file-path)
  "Prompt the minibuffer to save the current buffer as FILE-PATH."
  (interactive "bName of file to save: ")
  (save-buffer file-path)
  (message "File %s saved." file-path))

(defun joe-open ()
  "Insert a newline character."
  (interactive)
  (insert "\n"))

(defun joe-insc ()
  "Insert a space character."
  (interactive)
  (insert " "))

(defun joe-nextw ()
  "Move to the next window."
  (interactive)
  (other-window 1))

(defun joe-prevw ()
  "Move to the next window."
  (interactive)
  (other-window -1))

(defun joe-setmark (sid)
  "Set mark SID."
  (interactive "sSet Mark (0-9): ")
  (let ((id (string-to-number sid)))
    (cond
     ((= id 0)
      (progn
        (setq joe-mark-0 (point-marker))
        (message "Mark 0 set.")))
     ((= id 1)
      (progn
        (setq joe-mark-1 (point-marker))
        (message "Mark 1 set.")))
     ((= id 2)
      (progn
        (setq joe-mark-2 (point-marker))
        (message "Mark 2 set.")))
     ((= id 3)
      (progn
        (setq joe-mark-3 (point-marker))
        (message "Mark 3 set.")))
     ((= id 4)
      (progn
        (setq joe-mark-4 (point-marker))
        (message "Mark 4 set.")))
     ((= id 5)
      (progn
        (setq joe-mark-5 (point-marker))
        (message "Mark 5 set.")))
     ((= id 6)
      (progn
        (setq joe-mark-6 (point-marker))
        (message "Mark 6 set.")))
     ((= id 7)
      (progn
        (setq joe-mark-7 (point-marker))
        (message "Mark 7 set.")))
     ((= id 8)
      (progn
        (setq joe-mark-8 (point-marker))
        (message "Mark 8 set.")))
     ((= id 9)
      (progn
        (setq joe-mark-9 (point-marker))
        (message "Mark 9 set."))))))

(defun joe-gomark0 ()
  "Goto mark 0."
  (interactive)
  (if joe-mark-0
      (progn
        (push-mark)
        (goto-char joe-mark-0))
    (error "Mark 0 not set")))
(defun joe-gomark1 ()
  "Goto mark 1."
  (interactive)
  (if joe-mark-1
      (progn
        (push-mark)
        (goto-char joe-mark-1))
    (error "Mark 1 not set")))
(defun joe-gomark2 ()
  "Goto mark 2."
  (interactive)
  (if joe-mark-2
      (progn
        (push-mark)
        (goto-char joe-mark-2))
    (error "Mark 2 not set")))
(defun joe-gomark3 ()
  "Goto mark 3."
  (interactive)
  (if joe-mark-3
      (progn
        (push-mark)
        (goto-char joe-mark-3))
    (error "Mark 3 not set")))
(defun joe-gomark4 ()
  "Goto mark 4."
  (interactive)
  (if joe-mark-4
      (progn
        (push-mark)
        (goto-char joe-mark-4))
    (error "Mark 4 not set")))
(defun joe-gomark5 ()
  "Goto mark 5."
  (interactive)
  (if joe-mark-5
      (progn
        (push-mark)
        (goto-char joe-mark-5))
    (error "Mark 5 not set")))
(defun joe-gomark6()
  "Goto mark 6."
  (interactive)
  (if joe-mark-6
      (progn
        (push-mark)
        (goto-char joe-mark-6))
    (error "Mark 6 not set")))
(defun joe-gomark7 ()
  "Goto mark 7."
  (interactive)
  (if joe-mark-7
      (progn
        (push-mark)
        (goto-char joe-mark-7))
    (error "Mark 7 not set")))
(defun joe-gomark8 ()
  "Goto mark 8."
  (interactive)
  (if joe-mark-8
      (progn
        (push-mark)
        (goto-char joe-mark-8))
    (error "Mark 8 not set")))
(defun joe-gomark9 ()
  "Goto mark 9."
  (interactive)
  (if joe-mark-9
      (progn
        (push-mark)
        (goto-char joe-mark-9))
    (error "Mark 9 not set")))

(defun joe-gonextmark ()
  "Move point to next mark."
  (interactive)
  (if joe-lastmark
      (progn
        (push-mark)
        (goto-char joe-lastmark))))
(defun joe-goprevmark ()
  "Move point to next mark."
  (interactive)
  (if joe-nextmark
      (progn
        (push-mark)
        (goto-char joe-nextmark))))

; TODO fix bug where this only works if called from mb
(defun joe-run (com)
  "Append the output of shell command COM to current buffer."
  (interactive "sProgram to run: ")
  (let* ((joe-cur-mark (point-marker)))
    (call-interactively 'end-of-buffer)
    (insert (shell-command-to-string com))
    (goto-char joe-cur-mark)))

(defun joe-ffirst (str action)
  "Find next STR, perform ACTION."
  (interactive (list (joe-get-findstr) (joe-get-find-action)))
  (setq joe-prev-search str)
  (cond ((string= action "R")
         (progn
           (message "TODO")))
        ((string= action "B")
         (search-backward str))
        ((string= action "K")
         (progn
           (call-interactively 'narrow-to-region)
           (goto-char (point-min))
           (search-forward str)
           (widen)))
        ((search-forward str))))

(defalias 'joe-nbuf 'next-buffer)
(defalias 'joe-pbuf 'previous-buffer)
(defalias 'joe-reload 'revert-buffer)
(defalias 'joe-tw0 'delete-window)
(defalias 'joe-tw1 'delete-other-windows)

;;; setting joestar's wordstar-like keybindings
(defvar  joestar-mode-map
  (let ((joe-map (make-sparse-keymap)))
    ;; region
    (define-key joe-map (kbd "<C-right>") 'joe-ctr-selection-right)
    (define-key joe-map (kbd "<C-left>") 'joe-ctr-selection-left)
    (define-key joe-map (kbd "<C-up>") 'joe-ctr-selection-up)
    (define-key joe-map (kbd "<C-down>") 'joe-ctr-selection-down)
    (define-key joe-map (kbd "C-k C-y") 'delete-region)
    (define-key joe-map (kbd "C-k y") (kbd "C-k C-y"))
    (define-key joe-map (kbd "C-k C-m") 'joe-todo-func)
    (define-key joe-map (kbd "C-k m") (kbd "C-k C-m"))
    (define-key joe-map (kbd "C-k C-c") 'copy-region-as-kill) ;; TODO test
    (define-key joe-map (kbd "C-k c") (kbd "C-k C-c"))
    (define-key joe-map (kbd "C-k C-/") 'joe-todo-func)
    (define-key joe-map (kbd "C-k /") (kbd "C-k C-/"))
    (define-key joe-map (kbd "C-k C-w") '(lambda (file-path)
                                           "Writes the region to FILE-PATH."
                                           (interactive "FName of file to write (C-k h for help): ")
                                           (write-region (region-beginning) (region-end) file-path))) ; TODO contextual K-h keybind, and save if no block
    ;; had to define the lambda twice because (kbd C-k C-w) wasn't working for some reason
    (define-key joe-map (kbd "C-k w") '(lambda (file-path)
                                           "Writes the region to FILE-PATH."
                                           (interactive "FName of file to write (C-k h for help): ")
                                           (write-region (region-beginning) (region-end) file-path)))

    
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
    (define-key joe-map (kbd "C-k C-l") 'joe-line)
    (define-key joe-map (kbd "C-k l") (kbd "C-k C-l"))
    (define-key joe-map (kbd "C-g") 'joe-todo-func) ; TODO
    
    ;; misc
    (define-key joe-map (kbd "C-k C-j") 'joe-todo-func)
    (define-key joe-map (kbd "C-k j") (kbd "C-k C-j"))
    (define-key joe-map (kbd "C-k C-a") 'center-line)
    (define-key joe-map (kbd "C-k a") (kbd "C-k C-a"))
    (define-key joe-map (kbd "<C-k C-space>") 'joe-todo-func)
    (define-key joe-map (kbd "<C-k space>") (kbd "<C-k C-space>"))

    ;; spell
    (define-key joe-map (kbd "<escape> n") 'flyspell-word)
    (define-key joe-map (kbd "<escape> l") 'flyspell-buffer)

    ;; delete
    (define-key joe-map (kbd "<backspace>") 'joe-backspace)
    (define-key joe-map (kbd "C-y") 'kill-whole-line)
    (define-key joe-map (kbd "C-w") 'kill-word)
    (define-key joe-map (kbd "C-o") 'joe-delete-word-left)
    (define-key joe-map (kbd "<escape> o") '(lambda ()
                                         "Kill to the beginning of the line before point."
                                         (kill-line 0)))
    (define-key joe-map (kbd "C-^") 'joe-redo)
    (define-key joe-map (kbd "C-_") 'joe-undo)

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
    (define-key joe-map (kbd "C-k C-r") 'joe-insf)
    (define-key joe-map (kbd "C-k r") (kbd "C-k C-r"))
    (define-key joe-map (kbd "C-k C-d") 'joe-save)
    (define-key joe-map (kbd "C-k d") (kbd "C-k C-d"))
    (define-key joe-map (kbd "C-k C-`") 'revert-buffer)
    (define-key joe-map (kbd "C-k `") (kbd "C-k C-`"))
    (define-key joe-map (kbd "C-c") 'joe-tw0)
    
    ;; search
    (define-key joe-map (kbd "C-k C-f") 'joe-ffirst)
    (define-key joe-map (kbd "C-k f") (kbd "C-k C-f"))

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
    (define-key joe-map (kbd "C-]") 'joe-open)
    (define-key joe-map (kbd "C-@") 'joe-insc)
    (define-key joe-map (kbd "C-q") '(lambda ()(interactive))) ; TODO
    (define-key joe-map (kbd "<escape> y") '(lambda () (interactive))) ; TODO

    ;; window
    (define-key joe-map (kbd "C-k C-o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k o") (kbd "C-k C-o"))
    (define-key joe-map (kbd "C-k C-g") 'enlarge-window)
    (define-key joe-map (kbd "C-k g") (kbd "C-k C-g"))
    (define-key joe-map (kbd "C-k C-t") 'shrink-window)
    (define-key joe-map (kbd "C-k t") (kbd "C-k C-t"))
    (define-key joe-map (kbd "C-k C-n") 'joe-nextw)
    (define-key joe-map (kbd "C-k n") (kbd "C-k C-n"))
    (define-key joe-map (kbd "C-k C-p") 'joe-prevw)
    (define-key joe-map (kbd "C-k p") (kbd "C-k C-p"))
    (define-key joe-map (kbd "C-k C-i") '(lambda ()
                                           "Show one / All."
                                           (interactive))) ; TODO, cuz I don't even know what it does in joe
    (define-key joe-map (kbd "C-k i") (kbd "C-k C-i"))
    (define-key joe-map (kbd "M-+")  'text-scale-increase)
    (define-key joe-map (kbd "M--")  'text-scale-decrease)
    

    ;; shell TODO
    ;; in joe, the cursor does not change when the comand is appended.
    (define-key joe-map (kbd "<escape> !") 'joe-run)
    (define-key joe-map (kbd "C-k C-z") 'suspend-emacs)
    (define-key joe-map (kbd "C-k z") (kbd "C-k C-z"))

    ;; bookmark
    (define-key joe-map (kbd "<escape> <escape>") 'joe-setmark)
    (define-key joe-map (kbd "<escape> 0") 'joe-gomark0)
    (define-key joe-map (kbd "<escape> 1") 'joe-gomark1)
    (define-key joe-map (kbd "<escape> 2") 'joe-gomark2)
    (define-key joe-map (kbd "<escape> 3") 'joe-gomark3)
    (define-key joe-map (kbd "<escape> 4") 'joe-gomark4)
    (define-key joe-map (kbd "<escape> 5") 'joe-gomark5)
    (define-key joe-map (kbd "<escape> 6") 'joe-gomark6)
    (define-key joe-map (kbd "<escape> 7") 'joe-gomark7)
    (define-key joe-map (kbd "<escape> 8") 'joe-gomark8)
    (define-key joe-map (kbd "<escape> 9") 'joe-gomark9)
    
    (define-key joe-map (kbd "C-k C--") 'joe-gonextmark)
    (define-key joe-map (kbd "C-k -") (kbd "C-k C--"))
    (define-key joe-map (kbd "C-k C-=") 'joe-goprevmark)
    (define-key joe-map (kbd "C-k =") (kbd "C-k C-="))
    ;; buffer
    (define-key joe-map (kbd "<escape> u") 'joe-nbuf)
    (define-key joe-map (kbd "<escape> v") 'joe-pbuf)
    
    
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
