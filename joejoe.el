(defun my/prompt-save ()
  "Prompt the minibuffer to save a file."
  (interactive)
  (message "Name of file to save: %s" (buffer-name))
  (save-current-buffer)
  (message "%s saved." (buffer-name)))

(defun my/delete-window ()
  "Delete current window, exit Emacs if only one window."
  (interactive))

;;; some wordstar keybindings to make manipulating the editor itself a bit easier                       
(defvar my-keys-minor-mode-map                                                                          
  (let ((map (make-sparse-keymap)))                                                                     
    (define-key map (kbd "C-k o") 'split-window-vertically)
	(define-key map (kbd "C-k C-c") 'my/delete-window)
	(define-key map (kbd "C-k c") 'my/delete-window)
    (define-key map (kbd "C-k C-o") 'split-window-vertically)
	(define-key map (kbd "C-k v") 'split-window-horizontally)
	(define-key map (kbd "C-k C-v") 'split-window-horizontally)
	(define-key map (kbd "C-k b") 'kill-buffer)
	(define-key map (kbd "C-k n") 'other-window)
	(define-key map (kbd "C-k C-b") 'kill-buffer)
	(define-key map (kbd "C-k C-n") 'other-window)
	(define-key map (kbd "M-+")  'text-scale-increase)
	(define-key map (kbd "M--")  'text-scale-decrease)
    (define-key map (kbd "C-k C-d") 'my/prompt-save)
    (define-key map (kbd "C-k d") 'my/prompt-save)
    (define-key map (kbd "C-k u") 'beginning-of-buffer)
    (define-key map (kbd "C-k C-u") 'beginning-of-buffer)
    (define-key map (kbd "C-k v") 'end-of-buffer)
    (define-key map (kbd "C-k C-v") 'end-of-buffer)
    (define-key map (kbd "C-k u") 'beginning-of-buffer)
    (define-key map (kbd "C-k u") 'beginning-of-buffer)
    map)
  "My-keys-minor-mode keymap.")

(provide 'joejoe)
;;; joejoe.el ends here
