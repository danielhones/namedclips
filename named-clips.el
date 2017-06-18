(setq *named-clips-table* (make-hash-table :test 'equal))
(setq named-clips-keybinding-prefix "C-c n")


(defun nclip-add-clip-with-name (name clip)
  (puthash name clip *named-clips-table*))


(defun nclip-get-clip-with-name (name)
  (gethash name *named-clips-table* ""))


(defun nclip-remove-clip-with-name (name)
  (interactive
   (list (completing-read "Remove clip named: " (nclip-clip-names))))
  (remhash name *named-clips-table*)
  (message (concat "Removed clip named " name)))


(defun nclip-clear-all-named-clips (confirm)
 (interactive (list (y-or-n-p "Really clear all named clips? ")))
  (when confirm
    (clrhash *named-clips-table*)
    (message "Cleared table of named clips")))


(defun nclip-clip-names ()
  (let ((clip-names '()))
    (maphash (lambda (k _) (push k clip-names)) *named-clips-table*)
    clip-names))


(defun nclip-show-all-named-clips ()
  (interactive)
  ;; TODO: it would be nice if these listed either in alphabetical order
  ;;       or reverse chronological order by the order they were defined
  (let ((nc-buffer (get-buffer-create "*Named Clips*"))
        (nc-window (get-buffer-window "*Named Clips*"))
        (named-clips '()))
    (if (= 0 (length (nclip-clip-names))) (message "No named clips")
      (display-buffer nc-buffer)
      (with-current-buffer nc-buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (font-lock-mode)
        (let* ((parts '())
               (n (* (window-width) .7))
               (separator (concat (mapconcat 'identity (dotimes (_ n parts) (push "_" parts)) "") "\n")))
          (insert separator)
          (maphash (lambda (k v)
                     (insert (propertize (format "%s:\n" k) 'font-lock-face 'bold))
                     (insert (propertize (format "%s\n" v) 'font-lock-face 'normal))
                     (insert separator))
                   *named-clips-table*))
        (setq buffer-read-only t)))))


(defun nclip-name-last-kill-text (name)
  (interactive "sName for clip: ")
  (let ((clip (substring-no-properties (current-kill 0 t))))
        (nclip-add-clip-with-name name clip)
        (message (concat "Copied clip to " name))))


(defun nclip-name-region-as-clip (name)
  ;; TODO: need to check if region is even active, skip prompt and show message if not
  (interactive "sName for clip: ")
  (let ((clip (buffer-substring-no-properties (region-beginning) (region-end)))
        (mark (mark t))
        (point (point)))
    (nclip-add-clip-with-name name clip)
    (deactivate-mark)
    (set-marker (mark-marker) (point) (current-buffer))
    (goto-char mark)
    (set-marker (mark-marker) mark (current-buffer))
    (goto-char point)
    (message (concat "Copied region to " name))))


(defun nclip-name-region-or-last-kill (name)
  (interactive "sName for clip: ")
  (if (region-active-p) (nclip-name-region-as-clip name)
    (nclip-name-last-kill-text name)))


(defun nclip-insert-named-clip (name)
  (interactive
   (list (completing-read "Name of clip: " (nclip-clip-names))))
  (insert (nclip-get-clip-with-name name)))


(defun nclip-set-keybindings (&optional prefix)
  (let ((kb-prefix (or prefix named-clips-keybinding-prefix)))
    (global-set-key (kbd (concat kb-prefix " n")) 'nclip-name-region-or-last-kill)
    (global-set-key (kbd (concat kb-prefix " w")) 'nclip-name-region-as-clip)
    (global-set-key (kbd (concat kb-prefix " k")) 'nclip-name-last-kill-text)
    (global-set-key (kbd (concat kb-prefix " i")) 'nclip-insert-named-clip)
    (global-set-key (kbd (concat kb-prefix " l")) 'nclip-show-all-named-clips)
    (global-set-key (kbd (concat kb-prefix " r")) 'nclip-remove-clip-with-name)
    (global-set-key (kbd (concat kb-prefix " X")) 'nclip-clear-all-named-clips)))


(nclip-set-keybindings)
