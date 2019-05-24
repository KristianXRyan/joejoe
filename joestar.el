;;; package --- summary

;;; Commentary:

;;; TODO: Confine buffers to certain windows; better relative line number support with goto-line
;;; TODO: remake with lambdas

;;; Code:

;;; functions to change the behavior of keymaps


;;; set mark variables

;; TODO make a better system
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

;;; joestar-mode functions:

;; set mark funcs:

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


(defun joe/help-me ()
  "Prints the help documentation in a separate buffer.")

;;; TODO fix bug where ivy does not display the full file-name
(defun joe/save-file (file-path)
  "Prompt the minibuffer to save the current buffer as FILE-PATH."
  (interactive "FName of file to save: ")
  (save-buffer file-path)
  (message "%s saved." file-path))

(defun joe/delete-window ()
  "Delete current window, exit Emacs if only one window."
  (interactive)
  (if (= 1 (length (window-list)))
      (if (buffer-modified-p)
          (if (y-or-n-p "Lose changes to this file? ")
              (kill-emacs)
            nil)
        (kill-emacs))
    (delete-window)))

(defun joe/insert-file (file-path)
  "Prompt the minibuffer to save the current buffer as FILE-PATH."
  (interactive "FName of file to insert: ")
  (insert-file-contents file-path))

(defun joe/scroll-down ()
  "Scroll down while preserving the cursor point."
  (interactive)
  (scroll-down 1))

(defun joe/scroll-up ()
  "Scroll down while preserving the cursor point."
  (interactive)
  (scroll-up 1))

(defun joe/goto-abosolute-line (num)
  "Goto absolute line number NUM."
  (interactive "sGo to line: ")
  (forward-line (* -1 (count-lines 1 (point))))
  (forward-line (1- (string-to-number num))))

(defun joe/enlarge-window ()
  "Enlarge the current window.")

(defun joe/shrink-window ()
  "Shrink the current window.")

(defun joe/next-window ()
  "Move to the next window."
  (interactive)
  (other-window 1))

(defun joe/previous-window ()
  "Move to the previous window."
  (interactive)
  (other-window -1))

(defun joe/backward-delete-char ()
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

(defun joe/begin-selection ()
  "Begin the mark selection."
  (interactive)
  (set-mark-command)
  (keyboard-quit))

(defun joe/end-selectioin-and-select ()
  "End the mark selection and select everything between it and the mark."
  nil)

(defun joe/control-select-right ()
  "Control selection to right char."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'forward-char))


(defun joe/control-select-left ()
  "Control selection to left char."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'backward-char))

(defun joe/control-select-up ()
  "Control selection up one line."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'previous-line))

(defun joe/control-select-down ()
  "Control selection down one line."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'next-line))

(defun joe/kill-prev-word ()
  "Kill the word to the left of the cursor."
  (interactive)
  (call-interactively 'backward-word)
  (call-interactively 'kill-word))

;;; setting joestar's wordstar-like keybindings
(defvar  joestar-mode-map
  (let ((joe-map (make-sparse-keymap)))
    
    ;; buffer movement
    (define-key joe-map (kbd "C-u") 'scroll-down)
    (define-key joe-map (kbd "C-v") 'scroll-up)
    (define-key joe-map (kbd "C-e") 'end-of-line)
    (define-key joe-map (kbd "C-a") 'beginning-of-line-text)
    (define-key joe-map (kbd "C-k u") 'beginning-of-buffer)
    (define-key joe-map (kbd "C-k C-u") 'beginning-of-buffer)
    (define-key joe-map (kbd "C-k v") 'end-of-buffer)
    (define-key joe-map (kbd "C-k C-v") 'end-of-buffer)
    (define-key joe-map (kbd "C-z") 'backward-word)
    (define-key joe-map (kbd "C-x") 'forward-word)
    (define-key joe-map (kbd "C-k C-l") 'joe/goto-abosolute-line)
    (define-key joe-map (kbd "C-k l") 'joe/goto-abosolute-line)

    ;; buffer manipulation
    (define-key joe-map (kbd "C-k e") 'find-file)
    (define-key joe-map (kbd "C-k C-e") 'find-file)
    (define-key joe-map (kbd "C-y") 'kill-whole-line)
    (define-key joe-map (kbd "C-j") 'kill-line)
    (define-key joe-map (kbd "C-[ C-o") 'kill-line) ;; TODO
    (define-key joe-map (kbd "C-[ o") 'kill-line) ;; TODO
    (define-key joe-map (kbd "C-w") 'kill-word)
    (define-key joe-map (kbd "C-o") 'joe/kill-prev-word) ;; TODO, Test
    (define-key joe-map (kbd "C-^") 'redo)
    (define-key joe-map (kbd "C-k C-x") 'save-buffers-kill-emacs)
    (define-key joe-map (kbd "C-f") 'search-forward)
    (define-key joe-map (kbd "C-j") 'kill-line)
    (define-key joe-map (kbd "C-k r") 'joe/insert-file)
    (define-key joe-map (kbd "C-k C-r") 'joe/insert-file)
    (define-key joe-map (kbd "<backspace>") 'joe/backward-delete-char)
    (define-key joe-map (kbd "C-k b") 'joe/begin-selection)
    (define-key joe-map (kbd "C-k C-b") 'joe/begin-selection)
    (define-key joe-map (kbd "<C-right>") 'joe/control-select-right)
    (define-key joe-map (kbd "<C-left>") 'joe/control-select-left)
    (define-key joe-map (kbd "<C-up>") 'joe/control-select-up)
    (define-key joe-map (kbd "<C-down>") 'joe/control-select-down)
    (define-key joe-map (kbd "C-y") 'delete-region)
    (define-key joe-map (kbd "C-m") 'delete-region)
    
    
    ;; editor manipulation
    (define-key joe-map (kbd "C-c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k C-c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k C-o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k n") 'joe/next-window)
    (define-key joe-map (kbd "C-k C-n") 'joe/next-window)
    (define-key joe-map (kbd "C-k C-p") 'joe/previous-window)
    (define-key joe-map (kbd "C-k p") (kbd "C-k C-p"))
    (define-key joe-map (kbd "C-k C-d") 'joe/save-file)
    (define-key joe-map (kbd "C-k d") 'joe/save-file)
    (define-key joe-map (kbd "C-k g") 'enlarge-window)
    (define-key joe-map (kbd "C-k C-g") 'enlarge-window)
    (define-key joe-map (kbd "C-k t") 'shrink-window)
    (define-key joe-map (kbd "C-k C-t") 'shrink-window)
    
    ;; additions for features tha
    (define-key joe-map (kbd "M-+")  'text-scale-increase)
    (define-key joe-map (kbd "M--")  'text-scale-decrease)
    (define-key joe-map (kbd "C-k ;") 'split-window-horizontally)
    (define-key joe-map (kbd "C-k C-;") 'split-window-horizontally)

    ;; spellcheck TODO
    joe-map)
  "The joestar-mode keymaps.")

;;; defining the minor modes

(define-minor-mode joestar-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value nil
  :lighter " joe"
  nil
  
  ;;; setting default Emacs behaviour

  ;; smooth scroll.
  ;; https://www.emacswiki.org/emacs/SmoothScrolling
  (setq scroll-margin 1
        scroll-conservatively 0
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01)

  ;; keep cursor position while scrolling
  (setq scroll-preserve-screen-position 'always) ;; TODO fix behavior so that it always scrolls perfectly
  (setq scroll-error-top-bottom t))

(define-globalized-minor-mode global-joestar-mode joestar-mode
  (lambda () (joestar-mode 1)))

(provide 'joestar)
;;; joestar.el ends here
