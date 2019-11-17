;;; package --- summary Wordstar/joe compatibility mode.

;;; Copyright (C) Ryan Jeffrey 2019

;;; Author: Ryan Jeffrey <pwishie@gmail.com>
;;; Created: 2019-05-12
;;; Keywords: joe wordstar emulation editor
;;; Version: 0.5
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

;;; TODO: better relative line number support with goto-line

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
(defvar joe-prev-search-action nil "The last search action.")

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
(make-variable-buffer-local 'joe-prev-search-action)

;; non-interactive helper functions
;; these functions exist to break-up functionality and do not necessarily
;; correspond to any function in Joe.

; TODO fix bug where it does not keep the value of joe-prev-search beyond one repitition
(defun joe-get-findstr (prompt)
  "If PROMPT is t ask user for search words.  If PROMPT is nil return joe-prev-search."
  (setq joe-prev-search   (if (null joe-prev-search)
                              (read-string "Find: ")
                            (if prompt
                                (read-string (format "Find [%s]: " joe-prev-search))
                              joe-prev-search))))

(defun joe-get-find-action (prompt)
  "If PROMPT as the user for an action.  Otherwise, return previous action."
  (setq joe-prev-search-action (if (or prompt (null joe-prev-search))
                                   (read-string "(I)gnore (R)eplace (B)ackwards Bloc(K): ")
                                 joe-prev-search)))

(defun joe-find-do (action str)
  "Perform find ACTION on STR."
  (cond ((string= action "R") ; replace
         (joe-replace str (read-string "Replace with: ")))
        ((string= action "B") ; search backward
         (search-backward str))
        ((string= action "K") ; search block
         (progn
           (call-interactively 'narrow-to-region)
           (goto-char (point-min))
           (search-forward str)
           (widen)))
        ((search-forward str)))) ; default, search forward

(defun joe-replace (str repl)
  "Replace instances of STR with REPL."
  )

(defun joe-shift-region (distance)
  "Shift the region DISTANCE number of whitespace."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

;; This exists so that joe-filt can mark the entire buffer beforehand if necessary, just like JOE.
(defun joe-filter-command (com)
  "Joe filt helper func.  Run COM on shell."
  (interactive "sCommand to filter the region through: ")
  (shell-command-on-region (mark) (point) com (current-buffer) t))

;; aliases
(defalias 'joe-nbuf 'next-buffer)
(defalias 'joe-pbuf 'previous-buffer)
(defalias 'joe-reload 'revert-buffer)
(defalias 'joe-tw0 'delete-window)
(defalias 'joe-tw1 'delete-other-windows)
(defalias 'joe-paragraph 'fill-paragraph)
(defalias 'joe-isrch 'isearch-forward)
(defalias 'joe-notmod 'not-modified)
(defalias 'joe-retype 'redraw-frame)
(defalias 'joe-shell 'suspend-emacs)
(defalias 'joe-beep 'beep)
(defalias 'joe-pgup 'scroll-down-command)
(defalias 'joe-pgdn 'scroll-up-command)
(defalias 'joe-dnslide 'next-line)
(defalias 'joe-upslide 'previous-line)
(defalias 'joe-rsrch 'isearch-backward)
(defalias 'joe-dellin 'kill-whole-line)
(defalias 'joe-deleol 'kill-line)
(defalias 'joe-delw 'kill-word)
(defalias 'joe-exsave 'save-buffers-kill-emacs)
(defalias 'joe-querysave 'save-some-buffers)
(defalias 'joe-killjoe 'kill-emacs)
(defalias 'joe-nextword 'forward-word)
(defalias 'joe-prevword 'backward-word)
(defalias 'joe-tos 'beginning-of-buffer)
(defalias 'joe-bol 'beginning-of-line-text)
(defalias 'joe-uparw 'previous-line)
(defalias 'joe-eol 'end-of-line)
(defalias 'joe-home 'joe-bol)
(defalias 'joe-bof 'joe-tos)
(defalias 'joe-bos 'end-of-buffer)
(defalias 'joe-dnarw 'next-line)
(defalias 'joe-rtarw 'forward-char)
(defalias 'joe-ltarw 'backward-char)
(defalias 'joe-cd 'cd)
(defalias 'joe-savenow 'save-buffer)
(defalias 'joe-delch 'delete-char)

;; functions

(defun joe-byte (byte)
  "Go to byte BYTE."
  (interactive "nGo to byte: ")
  (goto-char byte))

(defun joe-lose ()
  "Kill buffer and replace it with a scratch buffer."
  (interactive)
  (let ((cur-buffer-name (buffer-name)))
    (kill-buffer)
    (if (string= cur-buffer-name (buffer-name))
        nil
      (joe-scratch "Unnamed"))))

(defun joe-col (col)
  "Go to column COL."
  (interactive "nGo to column: ")
  (move-to-column col t))

(defun joe-cancel ()
  "Escape."
  (interactive)
  (keyboard-escape-quit)
  (if (= (count-windows) 1)
      (when (y-or-n-p "Kill Emacs? ")
          (joe-killjoe))
    (joe-tw0)))

(defun joe-debug ()
  (interactive)
  (message "%d" (count-windows)))

(defun joe-rtn ()
  "Insert return key."
  (interactive)
  (insert (kbd "<RET>")))

(defun joe-txt (str)
  "Insert STR into buffer."
  (interactive "sInsert: ")
  (insert str))

(defun joe-name ()
  "Insert current file name into the buffer."
  (interactive)
  (if (buffer-file-name)
      (progn
        (insert (file-relative-name (buffer-file-name) default-directory)))
    (insert (buffer-name))))

(defun joe-language ()
  "Insert the language in the current buffer."
  (interactive)
  (insert current-language-environment))

(defun joe-charset ()
  "Insert the language in the current buffer."
  (interactive)
  (insert current-language-environment))

(defun joe-msg (str)
  "Display a message STR."
  (interactive "sMessage: ")
  (message "%s" str))

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


(defun joe-backs ()
  "Redefine the what the backspace key does to mimic joe."
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

(defun joe-backw ()
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

(defun joe-reloadall ()
  "Revert all buffers."
  (interactive)
  (let ((num-refreshed 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t)
          (setq num-refreshed (+ num-refreshed 1))))
      (message "%d files reloaded." num-refreshed))))

(defun joe-scratch (name)
  "Push a scratch buffer NAME into current window."
  (interactive "sName of buffer to edit: ")
  (generate-new-buffer (generate-new-buffer-name name))
  (switch-to-buffer name)
  (with-current-buffer name (text-mode)))

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
  "Insert a space."
  (interactive)
  (insert " "))

(defun joe-lindent ()
  "Indent region to the left."
  (interactive)
  (joe-shift-region (- tab-width)))

(defun joe-rindent ()
  "Indent region to the right."
  (interactive)
  (joe-shift-region  tab-width))

(defun joe-nextw ()
  "Move to the next window, or buffer if there is only one."
  (interactive)
  (if (= (length (window-list)) 1)
      (next-buffer)
    (other-window 1)))

(defun joe-prevw ()
  "Move to the previous window, or buffer if there is only one."
  (interactive)
    (if (= (length (window-list)) 1)
      (previous-buffer)
      (other-window -1)))

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

(defun joe-run (com)
  "Append the output of shell command COM to current buffer."
  (interactive "sProgram to run: ")
  (let* ((joe-cur-mark (point-marker)))
    (call-interactively 'end-of-buffer)
    (insert (shell-command-to-string com))
    (goto-char joe-cur-mark)))

(defun joe-qrepl (str)
  "Search and replace STR."
  (interactive (list (joe-get-findstr t)))
  (joe-find-do joe-prev-search-action str))

(defun joe-fnext (str action)
  "Repeat previous search on STR and perform previous ACTION."
  (interactive (list (joe-get-findstr nil) (joe-get-find-action nil)))
  (joe-find-do action str))

(defun joe-ffirst (str action)
  "Find next STR, perform ACTION."
  (interactive (list (joe-get-findstr t) (joe-get-find-action t)))
  (joe-find-do action str))

(defun joe-stat ()
  "Print status to the echo-area."
  (interactive)
  (message "%s Col %d %d(0x%x) %s" (what-line) (current-column) (buffer-size) (buffer-size) buffer-file-coding-system))

(defun joe-filt ()
  "Filter region through shell command."
  (interactive)
  (if (use-region-p)
      (call-interactively 'joe-filter-command)
    (progn
      (call-interactively 'mark-whole-buffer)
      (call-interactively 'joe-filter-command))))

(defun joe-delbol ()
  "Delete to the beginning of the line."
  (interactive)
  (kill-line 0))

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

    (define-key joe-map (kbd "C-k C-,") 'joe-lindent)
    (define-key joe-map (kbd "C-k ,") (kbd "C-k C-,"))
    (define-key joe-map (kbd "C-k C-.") 'joe-rindent)
    (define-key joe-map (kbd "C-k .") (kbd "C-k C-."))
    

    
    ;; goto
    (define-key joe-map (kbd "C-z") 'joe-prevword)
    (define-key joe-map (kbd "C-x") 'joe-nextword)
    (define-key joe-map (kbd "C-k C-u") 'joe-tos)
    (define-key joe-map (kbd "C-k u") (kbd "C-k C-u"))
    (define-key joe-map (kbd "C-k C-v") 'joe-bos)
    (define-key joe-map (kbd "C-k v") (kbd "C-k C-v"))
    (define-key joe-map (kbd "C-u") 'joe-pgup)
    (define-key joe-map (kbd "C-v") 'joe-pgdn)
    (define-key joe-map (kbd "C-e") 'joe-eol)
    (define-key joe-map (kbd "C-a") 'joe-bol)
    (define-key joe-map (kbd "C-k C-l") 'joe-line)
    (define-key joe-map (kbd "C-k l") (kbd "C-k C-l"))
    (define-key joe-map (kbd "C-g") 'joe-todo-func) ; TODO
    
    ;; misc
    (define-key joe-map (kbd "C-k C-j") 'joe-paragraph)
    (define-key joe-map (kbd "C-k j") (kbd "C-k C-j"))
    (define-key joe-map (kbd "C-k C-a") 'center-line)
    (define-key joe-map (kbd "C-k a") (kbd "C-k C-a"))
    (define-key joe-map (kbd "C-k C-SPC") 'joe-stat)
    (define-key joe-map (kbd "C-k SPC") (kbd "C-k SPC"))

    ;; spell
    (define-key joe-map (kbd "<escape> n") 'flyspell-word)
    (define-key joe-map (kbd "<escape> l") 'flyspell-buffer)

    ;; delete
    (define-key joe-map (kbd "<backspace>") 'joe-backs)
    (define-key joe-map (kbd "C-y") 'joe-dellin)
    (define-key joe-map (kbd "C-w") 'joe-delw)
    (define-key joe-map (kbd "C-o") 'joe-backw)
    (define-key joe-map (kbd "<escape> o") 'joe-delbol)
    (define-key joe-map (kbd "C-^") 'joe-redo)
    (define-key joe-map (kbd "C-_") 'joe-undo)
    (define-key joe-map (kbd "C-d") 'joe-delch)
    

    ;; exit
    (define-key joe-map (kbd "C-k C-x") 'joe-exsave)
    (define-key joe-map (kbd "C-k x") (kbd "C-k C-x"))
    (define-key joe-map (kbd "C-g") 'joe-cancel)
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
    (define-key joe-map (kbd "<escape> w") 'joe-upslide)
    (define-key joe-map (kbd "<escape> z") 'joe-dnslide)
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

    ;; shell TODO
    
    ;; in joe, the cursor does not change when the command is appended.
    (define-key joe-map (kbd "<escape> !") 'joe-run)
    (define-key joe-map (kbd "C-k C-z") 'joe-shell)
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
  (setq scroll-error-top-bottom t)

  (save-place-mode t)

  (unless (boundp 'joe-no-undo-tree)
    (undo-tree-mode t)))

(define-globalized-minor-mode global-joestar-mode joestar-mode
  (lambda () (joestar-mode 1)))

(provide 'joestar)
;;; joestar.el ends here
