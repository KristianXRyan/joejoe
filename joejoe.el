;;; package --- summary

;;; Commentary:

;;; TODO: Confine buffers to certain windows; better relative line number support with goto-line
;;; TODO: remake with lambdas

;;; Code:

;;; functions to change the behavior of keymaps
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


;;; setting joejoe's wordstar-like keybindings
(defvar  joejoe-mode-map
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
    (define-key joe-map (kbd "C-W") 'kill-word) ; Test
    (define-key joe-map (kbd "C-O") 'kill-word) ;; TODO, Test
    (define-key joe-map (kbd "C-^") 'redo)
    (define-key joe-map (kbd "C-k C-x") 'save-buffers-kill-emacs)
    (define-key joe-map (kbd "C-f") 'search-forward)
    (define-key joe-map (kbd "C-j") 'kill-line)
    (define-key joe-map (kbd "C-k r") 'joe/insert-file)
    (define-key joe-map (kbd "C-k C-r") 'joe/insert-file)
    (define-key joe-map (kbd "<backspace>") 'joe/backward-delete-char)
    
    ;; editor manipulation
    (define-key joe-map (kbd "C-c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k C-c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k c") 'joe/delete-window)
    (define-key joe-map (kbd "C-k C-o") 'split-window-vertically)
    (define-key joe-map (kbd "C-k b") 'kill-buffer)
    (define-key joe-map (kbd "C-k n") 'other-window)
    (define-key joe-map (kbd "C-k C-b") 'kill-buffer) ;; ???
    (define-key joe-map (kbd "C-k C-n") 'other-window)
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

    ;; region
    (define-key joe-map (kbd "<C-right>") 'highlight)
    
    ;; spellcheck TODO
    joe-map)
  "The joejoe-mode keymaps.")

;;; defining the minor modes

(define-minor-mode joejoe-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value nil
  :lighter " joe"
  :global t

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

(define-globalized-minor-mode global-joejoe-mode joejoe-mode
  (lambda () (joejoe-mode 1)))

(provide 'joejoe)
;;; joejoe.el ends here
