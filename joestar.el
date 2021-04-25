;;; package --- summary Wordstar/joe compatibility mode.

;;; Copyright (C) Ryan Jeffrey 2020

;;; Author: Ryan Jeffrey <pwishie@gmail.com>
;;; Created: 2019-05-12
;;; Keywords: joe wordstar emulation editor
;;; Version: 0.5.1
;;; Package-Requires: ((emacs "24.2") (undo-tree "0.8.5") (highlight))
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

(require 'highlight)
(require 'cl-lib)


;; block variables
(defvar joe-block-mark-start nil "Start of block.")
(defvar joe-block-mark-end   nil "End of block.")

(defvar joe-block-is         nil "If T block is set.")
(defvar joe-block-rect       nil "If T rectangular block operations.")
(defvar joe-block-show       nil "If T block is showed in blockface face.")

(defvar joe-stream           nil "Contains last defined Block.  Stream.")
(defvar joe-rect             nil "Contains last defined Block.  Rectangle.")

;; For converting the block to region.
(defvar joe-last-emacs-mark  nil "The last position of mark.")
(defvar joe-last-emacs-point nil "The last position of point.")

(defvar joe-ctrl-selection   nil "0 if not control-selecting, 1 if ctrl selecting.")

(make-variable-buffer-local 'joe-last-emacs-mark)
(make-variable-buffer-local 'joe-last-emacs-point)

;;; set mark variables
(defvar-local joe-mark-0 nil "Mark 0.")
(defvar-local joe-mark-1 nil "Mark 1.")
(defvar-local joe-mark-2 nil "Mark 2.")
(defvar-local joe-mark-3 nil "Mark 3.")
(defvar-local joe-mark-4 nil "Mark 4.")
(defvar-local joe-mark-5 nil "Mark 5.")
(defvar-local joe-mark-6 nil "Mark 6.")
(defvar-local joe-mark-7 nil "Mark 7.")
(defvar-local joe-mark-8 nil "Mark 8.")
(defvar-local joe-mark-9 nil "Mark 9.")

(defvar-local joe-lastmark nil "The last mark id.")
(defvar-local joe-nextmark nil "The next mark id.")

(defvar-local joe-marklist nil "List of the currently used marks.")

;; Search and query options.
(cl-defstruct (search-obj (:constructor search-obj-create)
                          (:copier nil))
  (str-q "" :documentation "Query string." :type string)
  (str-r "" :documentation "Replace string." :type string)
  (backwards nil :documentation "The direction of the search and replace." :type symbol)
  (a nil :documentation "If the query should include all files." :type symbol)
  (e nil :documentation "Search includes make error or grep buffers." :type symbol)
  (icase nil :documentation "If the search should ignore case." :type symbol)
  (replace nil :documentation "If the search is for replacing strings." :type symbol)
  (b nil :documentation "If the search is for replacing strings." :type symbol)
  (nnn -1 :documentation "Specified line number of query." :type number))



(defvar joe-prev-search (search-obj-create) "The last searched-for item.")
(defvar joe-prev-search-action nil "The last search action.")

(defvar joe-macro-0 nil "Macro 0.")
(defvar joe-macro-1 nil "Macro 1.")
(defvar joe-macro-2 nil "Macro 2.")
(defvar joe-macro-3 nil "Macro 3.")
(defvar joe-macro-4 nil "Macro 4.")
(defvar joe-macro-5 nil "Macro 5.")
(defvar joe-macro-6 nil "Macro 6.")
(defvar joe-macro-7 nil "Macro 7.")
(defvar joe-macro-8 nil "Macro 8.")
(defvar joe-macro-9 nil "Macro 9.")

;; non-interactive helper functions
;; these functions exist to break-up functionality and do not necessarily
;; correspond to any function in Joe.

(defun joe-get-findstr ()
  "Ask user for search words."
  (setf (search-obj-str-q joe-prev-search)
        (if (or (null joe-prev-search) (string= "" (search-obj-str-q joe-prev-search)))
            (read-string "Find: ")
          (let ((new-search (read-string (format "Find [%s]: " (search-obj-str-q joe-prev-search)))))
            (if (or (null new-search) (string= new-search ""))
                (search-obj-str-q joe-prev-search)
              new-search)))))

;; Buffer location for the below joe-replace code.
(defun joe-replace (pos &optional noask prev-edits)
  "Prompt the user for an action and replace str-q with str-r at POS.
If NOASK is set just replace string without asking the user.
If BACK is t then move backwards, if nil then forwards.
PREV-EDITS is a list of where previous edits occurred."
  (let* ((str-r (search-obj-str-r joe-prev-search))
         (str-q (search-obj-str-q joe-prev-search))
         (back (search-obj-backwards joe-prev-search))
         (reg-max (if back
                      (+ (point) (length str-q))
                    (point)))
         (reg-min (- reg-max (length str-q)))
         (raw-user-answer
          (progn ; Highlighted before user is queried.
            (hlt-highlight-region reg-min reg-max 'highlight)
            (read-key "Replace (Y)es (N)o (R)est (B)ackup?")))
         (user-answer (if (numberp raw-user-answer)
                          (upcase raw-user-answer)
                        raw-user-answer))
         (next-call (if back
                        (lambda ()
                          (search-backward str-q)
                          (search-forward str-q))
                      (lambda ()
                        (search-forward str-q)))))
    
    (hlt-unhighlight-region reg-min reg-max 'highlight)
    (cond ((symbolp user-answer)
           ;; TODO do key event (like left, right, etc.)
           nil)
           ((= user-answer ?Y) (progn
                                (kill-region reg-max reg-min)
                                (insert str-r)
                                (joe-replace (funcall next-call)
                                             noask (cons reg-max prev-edits))))
          ;; Do not modify the selection, just go to next find.
          ((= user-answer ?N)   (joe-replace (funcall next-call)
                                             nil (cons reg-max prev-edits)))
          ;; Go back. Undo the last deletion.
          ;; TODO maybe use undo-tree for this
          ((= user-answer ?B) (let ((last-locale (car prev-edits))
                                    (str-r-len (* (length str-r) (if back -1 1)))
                                    (str-q-len (* (length str-q) (if back 1 -1))))
                                
                                (when (not (null last-locale))
                                  (goto-char (+ (point) str-q-len))
                                  (goto-char (+  last-locale str-q-len))
                                  (kill-region (point) (+ (point) str-r-len))
                                  (insert str-q)
                                  (joe-replace (point) back (cdr prev-edits))))))))

;;TODO fix minor bug where "backing" to the last entry takes the point too far.
(defun joe-replace-og (pos &optional noask back prev-edits)
  "Prompt user, replace STR1 with (SEARCH-OBJ-STR-R JOE-PREV-SEARCH) at POS.
If NOASK is set just replace string without asking the user.
If BACK is t then move backwards, if nil then forwards.
PREV-EDITS is a list of where previous edits occurred."
  (when (search-obj-str-q joe-prev-search)
    (hlt-highlight-region (- (point) (length (search-obj-str-q joe-prev-search))) (point) 'highlight)
    (let* ((reg-min (- (point) (length (search-obj-str-q joe-prev-search))))
           (result (if noask
                       ?Y
                     (upcase (read-key "Replace (Y)es (N)o (R)est (B)ackup?"))))
           (point-at-begin (+ (point) (length (search-obj-str-q joe-prev-search))))
           (next-call (if back
                          (lambda ()
                            (search-backward (search-obj-str-q joe-prev-search))
                            (search-forward (search-obj-str-q joe-prev-search)))
                        (lambda ()
                          (search-forward (search-obj-str-q joe-prev-search)))))
           (cur-point (point)))
      
      (hlt-unhighlight-region reg-min (point) 'highlight)
      (cond ((= result ?Y) (progn
                             (kill-region (point) reg-min)
                             (insert (search-obj-str-r joe-prev-search))
                             (joe-replace (search-obj-str-r joe-prev-search) (search-obj-str-q joe-prev-search) (funcall next-call)
                                          noask back (cons (buf-location-create :position cur-point) prev-edits))))
            
            ((= result ?N) (joe-replace (search-obj-str-r joe-prev-search)
                                        (search-obj-str-q joe-prev-search)
                                        (funcall next-call) noask back prev-edits))
            ;; Undo the previous edits.
            ((= result ?B) 
             (let ((last-edit (if (car prev-edits)
                                  (car prev-edits)
                                (if back
                                    (point-max)
                                  (point-min)))))
               (hlt-unhighlight-region reg-min (point) 'highlight)
               (if back
                   (if (< last-edit (point))
                       nil
                     nil)
                 (goto-char (- (point) (length (search-obj-str-r joe-prev-search))))
                 (search-backward (search-obj-str-q joe-prev-search))
                 (if (> last-edit (point))
                     (progn
                       (goto-char last-edit)
                       (kill-region (point) (- (point) (length (search-obj-str-r joe-prev-search))))
                       (insert (search-obj-str-q joe-prev-search)))
                   (goto-char (+ (point) (length (search-obj-str-q joe-prev-search))))))
               (joe-replace (search-obj-str-r joe-prev-search) (search-obj-str-q joe-prev-search) (point) noask back (cdr prev-edits))))
            
            ((= result ?R) (progn
                             (hlt-unhighlight-region reg-min (point) 'highlight)
                             (replace-string (search-obj-str-q joe-prev-search) (search-obj-str-r joe-prev-search))))
            (t (message (format "%c" result))
               (hlt-unhighlight-region reg-min (point) 'highlight))))))

(defun joe-str-contains (needle haystack)
  "T if HAYSTACK contain NEEDLE.  NEEDLE could be a regexp or char."
  (not (null (string-match-p (if (numberp needle)
                                 (char-to-string needle)
                               needle)
                             haystack))))

(defun joe-get-find-action ()
  "Ask the user for an action."
  (setq joe-prev-search
        (let* ((in-string (upcase
                           (read-string
                            "(I)gnore (R)eplace (B)ackwards Bloc(K): ")))
               (first-numeric (string-match-p "[0-9]" in-string)))
          (if (string= "" in-string)
              (search-obj-create :str-q (search-obj-str-q joe-prev-search))
            (search-obj-create :str-q (search-obj-str-q joe-prev-search)
                               :backwards (joe-str-contains ?B in-string)
                               :a (joe-str-contains ?A in-string)
                               :e (joe-str-contains ?E in-string)
                               :icase (joe-str-contains ?I in-string)
                               :b (joe-str-contains ?K in-string)
                               :nnn (if first-numeric
                                        (cl-parse-integer (substring in-string first-numeric) :junk-allowed t)
                                      -1)
                               :replace (joe-str-contains ?R in-string))))))

(defun joe-find-do ()
  "Set up joe-replace."
  (let* ((str (search-obj-str-q joe-prev-search)))
    (cond ((search-obj-replace joe-prev-search) ; Replace.
           (setf (search-obj-str-r joe-prev-search) (read-string "Replace with: "))
           (if (search-obj-backwards joe-prev-search)
               (joe-replace (search-backward str) t)
             (joe-replace (search-forward str))))
          ((search-obj-backwards joe-prev-search) ; Search backward.
           (search-backward str))
          ((search-obj-b joe-prev-search)
           (progn
             (call-interactively 'narrow-to-region)
             (goto-char (point-min))
             (search-forward str)
             (widen)))
          (t (search-forward str))))) ; default, search forward

(defun joe-shift-region (distance)
  "Shift the region DISTANCE number of whitespace."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun joe-get-macro-from-char (name)
  "Get the macro variable corresponding to NAME."
  (cond ((= ?0 name) joe-macro-0)
        ((= ?1 name) joe-macro-1)
        ((= ?2 name) joe-macro-2)
        ((= ?3 name) joe-macro-3)
        ((= ?4 name) joe-macro-4)
        ((= ?5 name) joe-macro-5)
        ((= ?6 name) joe-macro-6)
        ((= ?7 name) joe-macro-7)
        ((= ?8 name) joe-macro-8)
        ((= ?9 name) joe-macro-9)
        (t nil)))

(defun joe-block-test ()
  "Test if there is a Block or Start/End in current buffer."
  (if (and joe-block-mark-start joe-block-mark-end)
      (if (not (= (- joe-block-mark-end joe-block-mark-start) 0))
	  ()
	(message "Zero length Block..."))
    (if joe-block-mark-start
	(error "End of Block not set")
      (if joe-block-mark-end
	  (error "Start of Block not set")
      (error "No Block")))))

(defun joe-block-to-region()
  "Convert the joe-block into region."
  (setq joe-last-emacs-mark (mark))
  (setq joe-last-emacs-point (point-marker))
  (set-mark joe-block-mark-start)
  (goto-char joe-block-mark-end))


(defun joe-region-to-block ()
  "Convert the Emacs region to a joestar block."
  (interactive)
  (let* ((call-local (point-marker)))
    (joe-markb)
    (goto-char (mark))
    (joe-markk)
    (goto-char call-local)))

(defun joe-restore-emacs-mark-and-point ()
  "Return mark and point to their original values."
  (goto-char joe-last-emacs-point)
  (set-mark joe-last-emacs-mark))

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
(defalias 'joe-groww 'enlarge-window)
(defalias 'joe-shrinkw 'shrink-window)
(defalias 'joe-edit 'find-file)
(defalias 'joe-bufed 'list-buffers)
(defalias 'joe-splitw 'split-window-vertically)
(defalias 'joe-splitwh 'split-window-horizontally)

;; functions

;;;;;;;;;;;;
;; BLOCKS ;;
;;;;;;;;;;;;

(defface blockface
 '((t :foreground  "gainsboro"
       :background "dim gray"
       ))
  "Face for joestar-blocks."
  :group 'joestar-mode)

(defun joe-block-unset ()
  "Unset Block in current buffer."
  (interactive)
;; normal face
  (if (and joe-block-mark-start joe-block-mark-end)
      (hlt-unhighlight-region joe-block-mark-start joe-block-mark-end 'highlight))
  (setq joe-block-mark-start nil
	joe-block-mark-end nil
	joe-block-is nil
	joe-block-show nil
    joe-ctrl-selection nil)
  (message "Block unset."))

(defun joe-block-new-start ()
  "Extend Block."
;  (interactive)
  (joe-block-test)
  (if (= (point) (or joe-block-mark-start joe-block-mark-end))
      (error "Same Block")
    (if ( < (point) joe-block-mark-end)
	(setq joe-block-mark-start (point-marker))
      (setq joe-block-mark-start joe-block-mark-end
	    joe-block-mark-end (point-marker)))
    (setq joe-rect (extract-rectangle joe-block-mark-start joe-block-mark-end)
	joe-stream (buffer-substring joe-block-mark-start joe-block-mark-end)
	joe-block-is t)
    (joe-refontify)
    (message "Block redefined. Copied to global register.")))

  
(defun joe-block-new-end ()
  "Extend Block."
;  (interactive)
  (joe-block-test)
  (if (= (point) (or joe-block-mark-start joe-block-mark-end))
      (error "Same Block")
    (if ( > (point) joe-block-mark-start)
	(setq joe-block-mark-end (point-marker))
      (setq joe-block-mark-end joe-block-mark-start
	    joe-block-mark-start (point-marker)))
    (setq joe-rect (extract-rectangle joe-block-mark-start joe-block-mark-end)
	joe-stream (buffer-substring joe-block-mark-start joe-block-mark-end)
	joe-block-is t)
    (joe-refontify)
    (message "Block redefined. Copied to global register.")))



(defun joe-markb (&optional arg)
  "Start of block.  With ARG redefine Start of Block."
  (interactive "P")
  (if arg
      (joe-block-new-start)
    (if joe-block-is
      (joe-block-unset))
    (if (null joe-block-mark-end)
	(progn
	  (setq joe-block-mark-start (point-marker))
	  (message "Block Start marked. Not End."))
      (if ( < (point-marker) joe-block-mark-end)
	    (setq joe-block-mark-start (point-marker))
	(setq joe-block-mark-start joe-block-mark-end
	      joe-block-mark-end (point-marker)))
      (setq joe-rect (extract-rectangle joe-block-mark-start joe-block-mark-end)
	    joe-stream (buffer-substring joe-block-mark-start joe-block-mark-end)
	    joe-block-is t
	    joe-block-show t)
      
      ;; default Block is highlighted in blockface face
      (hlt-highlight-region joe-block-mark-start joe-block-mark-end 'highlight))))

(defun joe-markk (&optional arg)
  "End of block.  With ARG redefine End of Block."
  (interactive "P")
  (if arg
      (joe-block-new-end) ; unset of prev block
    (if joe-block-is
	(joe-block-unset))
    (if (null joe-block-mark-start)
	(progn
	  (setq joe-block-mark-end (point-marker))
	  (message "Block End marked. Not Start."))
      (if ( > (point-marker) joe-block-mark-start)
	  (setq joe-block-mark-end (point-marker))
	(setq joe-block-mark-end joe-block-mark-start
	      joe-block-mark-start (point-marker)))
      (setq joe-rect (extract-rectangle joe-block-mark-start joe-block-mark-end)
	    joe-stream (buffer-substring joe-block-mark-start joe-block-mark-end)
	    joe-block-is t
	    joe-block-show t)
      (hlt-highlight-region joe-block-mark-start joe-block-mark-end 'highlight))))

(defun joe-markl ()
  "Mark the current line as a block."
  (interactive)
  (setq joe-last-emacs-point (point-marker))
  (joe-bol)
  (joe-markb)
  (joe-eol)
  (joe-markk)
  (goto-char joe-last-emacs-point))

(defun joe-blkmove ()
  "Move the block to point."
  (interactive)
  (joe-block-test)
  (let* ((blk-len (- joe-block-mark-start joe-block-mark-end))
         (pnt (point-marker)))
    (joe-block-to-region)
    (call-interactively 'kill-region)
    (goto-char pnt)
    (yank)
    (joe-markk)
    (goto-char (+ (point-marker) blk-len))
    (joe-markb)))

 ;; converts the block to region and then kills the region
(defun joe-blkdel () ; TODO option to not save the block in kill ring
  "Delete the block.
Move mark to joestar's end of block and move point to joestar's end of block."
  (interactive)
  (joe-block-test)
  (joe-block-to-region)
  (call-interactively 'kill-region)
  (joe-restore-emacs-mark-and-point))

(defun joe-blkcpy()
  "Copy the block at point." ; TODO optionally not into the kill ring
  (interactive)
  (joe-block-test)
  (let* ((blk-len (- joe-block-mark-start joe-block-mark-end)))
    (joe-block-to-region)
    (call-interactively 'copy-region-as-kill)
    (joe-restore-emacs-mark-and-point)
    (yank)
    (joe-markk)
    (goto-char (+ (point-marker) blk-len))
    (joe-markb)))

(defun joe-blksave (filepath)
  "Save the block to a text file FILEPATH.  If no block, save the whole buffer."
  (interactive "FName of file to write: ")
  (write-region joe-block-mark-start joe-block-mark-end filepath))


(defun joe-filt ()
  "Filter the block through a shell command and replace the block with its output." ; TODO whole buffer if no block
  (interactive)
  ;; check if block, if not, make buffer block
  (let* ((string  (read-from-minibuffer "Command to filter block through: "
                            nil nil nil
                            'shell-command-history)))
    (shell-command-on-region joe-block-mark-start joe-block-mark-end string t t)))


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


;; need to have the copied area as block

;; end

;; TODO does not work
(defun joe-record (name)
  "Record a keyboard macro and save it to NAME."
  (interactive "cMacro to Record (0-9): ")
  (let* ((macro (joe-get-macro-from-char name)))
    (call-interactively 'start-kbd-macro)
    (call-interactively 'kmacro-end-macro)
    (call-interactively 'name-last-kbd-macro)
    (call-interactively 'insert-kbd-macro)))

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
  (unless (keyboard-escape-quit)
    (if (= (count-windows) 2)
      (when (y-or-n-p "Kill Emacs? ")
          (joe-killjoe))
    (joe-tw0))))

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


(defun joe-line (num)
  "Goto absolute line number NUM."
  (interactive "sGo to line: ")
  (forward-line (* -1 (count-lines 1 (point))))
  (forward-line (1- (string-to-number num))))


(defun joe-backs ()
  "Redefine the what the backspace key does to mimic joe."
  (interactive)
  (if buffer-read-only                      ; do the normal backspacing if buffer is readonly
      (setq unread-command-events (listify-key-sequence (kbd "DEL")))
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

(defun joe-setmark (id)
  "Set mark SID."
  (interactive (list (read-key "sSet Mark (0-9): ")))
  (cond
   ((symbolp id)
    ;; TODO handle this.
    nil)
   ((= id ?0)
    (progn
      (setq joe-mark-0 (point-marker))
      (message "Mark 0 set.")))
   ((= id ?1)
    (progn
      (setq joe-mark-1 (point-marker))
      (message "Mark 1 set.")))
   ((= id ?2)
    (progn
      (setq joe-mark-2 (point-marker))
      (message "Mark 2 set.")))
   ((= id ?3)
    (progn
      (setq joe-mark-3 (point-marker))
      (message "Mark 3 set.")))
   ((= id ?4)
    (progn
      (setq joe-mark-4 (point-marker))
      (message "Mark 4 set.")))
   ((= id ?5)
    (progn
      (setq joe-mark-5 (point-marker))
      (message "Mark 5 set.")))
   ((= id ?6)
    (progn
      (setq joe-mark-6 (point-marker))
      (message "Mark 6 set.")))
   ((= id ?7)
    (progn
      (setq joe-mark-7 (point-marker))
      (message "Mark 7 set.")))
   ((= id ?8)
    (progn
      (setq joe-mark-8 (point-marker))
      (message "Mark 8 set.")))
   ((= id ?9)
    (progn
      (setq joe-mark-9 (point-marker))
      (message "Mark 9 set.")))))

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

(defun joe-fnext (str action)
  "Repeat previous search on STR and perform previous ACTION."
  (joe-find-do))

; TODO does not replace yet
(defun joe-ffirst ()
  "Find next STR, perform ACTION."
  (interactive)
  (joe-get-findstr)
  (joe-get-find-action)
  (joe-find-do))

(defun joe-stat ()
  "Print status to the echo-area."
  (interactive)
  (message "%s Col %d %d(0x%x) %s" (what-line) (current-column) (buffer-size) (buffer-size) buffer-file-coding-system))

(defun joe-delbol ()
  "Delete to the beginning of the line."
  (interactive)
  (kill-line 0))

;;; setting joestar's wordstar-like keybindings
(defvar  joestar-mode-map
  (let ((joe-map (make-sparse-keymap)))
    ;; block
    (define-key joe-map (kbd "C-k C-b") 'joe-markb)
    (define-key joe-map (kbd "C-k b") (kbd "C-k C-b"))
    (define-key joe-map (kbd "C-k C-k") 'joe-markk)
    (define-key joe-map (kbd "C-k k") (kbd "C-k C-k"))
    (define-key joe-map (kbd "<C-right>") 'joe-ctr-selection-right) ; TODO change these from region to block
    (define-key joe-map (kbd "<C-left>") 'joe-ctr-selection-left)
    (define-key joe-map (kbd "<C-up>") 'joe-ctr-selection-up)
    (define-key joe-map (kbd "<C-down>") 'joe-ctr-selection-down)
    (define-key joe-map (kbd "C-k C-y") 'joe-blkdel)
    (define-key joe-map (kbd "C-k y") (kbd "C-k C-y"))
    (define-key joe-map (kbd "C-k C-m") 'joe-blkmove)
    (define-key joe-map (kbd "C-k m") (kbd "C-k C-m"))
    (define-key joe-map (kbd "C-k C-c") 'joe-blkcpy)
    (define-key joe-map (kbd "C-k c") (kbd "C-k C-c"))
    (define-key joe-map (kbd "C-k C-/") 'joe-todo-func)
    (define-key joe-map (kbd "C-k /") (kbd "C-k C-/"))
    (define-key joe-map (kbd "C-k C-w") 'joe-blksave)
    (define-key joe-map (kbd "C-k w") 'joe-blksave) ; had to put function twice, it bugs out if i use kbd
    (define-key joe-map (kbd "C-k C-,") 'joe-lindent)
    (define-key joe-map (kbd "C-k ,") (kbd "C-k C-,"))
    (define-key joe-map (kbd "C-k C-.") 'joe-rindent)
    (define-key joe-map (kbd "C-k .") (kbd "C-k C-."))
    (define-key joe-map (kbd "C-.") 'joe-region-to-block) ; -- EXTRA
    
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
    ;(define-key joe-map (kbd "C-g") 'joe-cancel)
    (define-key joe-map (kbd "C-k C-q") 'kill-emacs)
    (define-key joe-map (kbd "C-k q") (kbd "C-k C-q"))

    ;; file
    (define-key joe-map (kbd "C-k C-e") 'joe-edit)
    (define-key joe-map (kbd "C-k e") (kbd "C-k C-e"))
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
    (define-key joe-map (kbd "C-k C-o") 'joe-splitw)
    (define-key joe-map (kbd "C-k o") (kbd "C-k C-o"))
    (define-key joe-map (kbd "C-k C-0") 'joe-splitwh)
    (define-key joe-map (kbd "C-k 0") (kbd "C-k C-0"))
    (define-key joe-map (kbd "C-k C-g") 'joe-groww) ; TODO horizontal window support
    (define-key joe-map (kbd "C-k g") (kbd "C-k C-g"))
    (define-key joe-map (kbd "C-k C-t") 'joe-shrinkw)
    (define-key joe-map (kbd "C-k t") (kbd "C-k C-t"))
    (define-key joe-map (kbd "C-k C-n") 'joe-nextw)
    (define-key joe-map (kbd "C-k n") (kbd "C-k C-n"))
    (define-key joe-map (kbd "C-k C-p") 'joe-prevw)
    (define-key joe-map (kbd "C-k p") (kbd "C-k C-p"))
    (define-key joe-map (kbd "C-k C-i") '(lambda ()
                                           "Show one / All."
                                           (interactive))) ; TODO, cuz I don't even know what it does in joe
    (define-key joe-map (kbd "C-k i") (kbd "C-k C-i"))

    ;; shell
    ;(define-key joe-map (kbd "<f1>") 'shell)
    
    
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
