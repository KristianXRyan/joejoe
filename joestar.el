;;; package --- summary Wordstar/joe compatibility mode.

;;; Copyright (C) Ryan Jeffrey 2019

;;; Author: Ryan Jeffrey <pwishie@gmail.com>
;;; Keywords: joe wordstar
;;; Version 0.1
;;; Package-Requires: ((emacs "24.2"))

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
    
    ;; buffer movement
    (define-key joe-map (kbd "C-u") 'scroll-down)
    (define-key joe-map (kbd "C-v") 'scroll-up)
    (define-key joe-map (kbd "C-e") 'end-of-line)
    (define-key joe-map (kbd "C-a") 'beginning-of-line-text)
    
    (define-key joe-map (kbd "C-k C-u") 'beginning-of-buffer)
    (define-key joe-map (kbd "C-k u") (kbd "C-k C-u"))
    
    (define-key joe-map (kbd "C-k C-v") 'end-of-buffer)
    (define-key joe-map (kbd "C-k v") (kbd "C-k C-v"))
    
    (define-key joe-map (kbd "C-z") 'backward-word)
    (define-key joe-map (kbd "C-x") 'forward-word)
    
    (define-key joe-map (kbd "C-k C-l") '(lambda (num)
                                           "Goto absolute line number NUM."
                                           (interactive "sGo to line: ")
                                           (forward-line (* -1 (count-lines 1 (point))))
                                           (forward-line (1- (string-to-number num)))))
    (define-key joe-map (kbd "C-k l") (kbd "C-k C-l"))

    ;; buffer manipulation
    (define-key joe-map (kbd "C-k C-e") 'find-file)
    (define-key joe-map (kbd "C-k e") (kbd "C-k e"))
    (define-key joe-map (kbd "C-y") 'kill-whole-line)
    
    (define-key joe-map (kbd "ESC C-o") 'kill-line)
    (define-key joe-map (kbd "ESC o") (kbd "ESC C-o"))
    
    (define-key joe-map (kbd "C-w") 'kill-word)
    (define-key joe-map (kbd "C-o") '(lambda ()
                                       "Kill the word before point."
                                       "Kill the word to the left of the cursor."
                                       (interactive)
                                       (call-interactively 'backward-word)
                                       (call-interactively 'kill-word))) ;; TODO, Test

    
    (define-key joe-map (kbd "C-^") '(lambda ()
                                       "Undo-tree redo."
                                       (call-interactively 'redo)))
    (define-key joe-map (kbd "C-_") '(lambda ()
                                       "Undo-tree undo."
                                       (call-interactively 'undo)))
    
    (define-key joe-map (kbd "C-k C-x") 'save-buffers-kill-emacs)
    (define-key joe-map (kbd "C-k C-x") (kbd "C-k C-x"))
    
    (define-key joe-map (kbd "C-f") 'search-forward)
    (define-key joe-map (kbd "C-j") 'kill-line)
    (define-key joe-map (kbd "ESC o") '(lambda ()
                                         "Kill to the beginning of the line before point."
                                         (kill-line 0)))
    
    (define-key joe-map (kbd "C-k C-r") '(lambda ()
                                           "Prompt the minibuffer to save the current buffer as FILE-PATH."
                                           (interactive "FName of file to insert: ")
                                           (insert-file-contents file-path)))
    (define-key joe-map (kbd "C-k r") (kbd "C-k C-r"))
    
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
                                           (interactive)))
    (define-key joe-map (kbd "C-k m") (kbd "C-k C-m"))
    
    ;; editor manipulation
    (define-key joe-map (kbd "C-c") '(lambda ()
                                       "Delete current window, exit Emacs if only one window."
                                       (interactive)
                                       (if (= 1 (length (window-list)))
                                           (if (buffer-modified-p)
                                               (if (y-or-n-p "Lose changes to this file? ")
                                                   (kill-emacs)
                                                 nil)
                                             (kill-emacs))
                                         (delete-window))))
    
    (define-key joe-map (kbd "C-k o") 'split-window-vertically)
    
    (define-key joe-map (kbd "C-k C-c") 'copy-region-as-kill) ;; TODO test
    (define-key joe-map (kbd "C-k c") (kbd "C-k C-c"))
    
    (define-key joe-map (kbd "C-k C-o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k o") (kbd "C-k C-o"))
    
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
    
    (define-key joe-map (kbd "C-k C-d") '(lambda (file-path)
                                           "Prompt the minibuffer to save the current buffer as FILE-PATH."
                                           (interactive "FName of file to save: ")
                                           (save-buffer file-path)
                                           (message "%s saved." file-path)))
    
    (define-key joe-map (kbd "C-k d") (kbd "C-c C-d"))
    
    (define-key joe-map (kbd "C-k C-g") 'enlarge-window)
    (define-key joe-map (kbd "C-k g") (kbd "C-k C-g"))
    
    (define-key joe-map (kbd "C-k C-t") 'shrink-window)
    (define-key joe-map (kbd "C-k t") (kbd "C-k C-t"))
    
    ;; additions for features that joe does not support.
    (define-key joe-map (kbd "M-+")  'text-scale-increase)
    (define-key joe-map (kbd "M--")  'text-scale-decrease)
    (define-key joe-map (kbd "C-k C-;") 'split-window-horizontally)
    (define-key joe-map (kbd "C-k ;") (kbd "C-k C-;"))

    ;; spellcheck TODO
    joe-map)
  "The joestar-mode keymaps.")

;;; defining the minor modes

(define-minor-mode joestar-mode
  "A minor mode so that my key settings override annoying major modes."
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
  (setq scroll-preserve-screen-position 'always) ; TODO fix behavior so that it always scrolls perfectly
  (setq scroll-error-top-bottom t)
  )

(define-globalized-minor-mode global-joestar-mode joestar-mode
  (lambda () (joestar-mode 1)))

(provide 'joestar)
;;; joestar.el ends here
