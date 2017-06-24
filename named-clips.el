(setq *named-clips-table* (make-hash-table :test 'equal))
(setq *named-clips-shortcuts* (make-hash-table :test 'equal))

(if (not (boundp 'named-clips-keybinding-prefix)) (setq named-clips-keybinding-prefix "C-c n"))


(defun nclip-add-clip-with-name (name clip)
  (puthash name clip *named-clips-table*))


(defun nclip-add-clip-with-name-check-exists (name clip)
  "Adds a named clip to the hash table but will prompt for confirmation when doing so will \
overwrite an existing entry in the table"
  (if (gethash name *named-clips-table*)
      (if (y-or-n-p "A clip with that name already exists. Overwrite? ")
          (nclip-add-clip-with-name name clip)
        (message "No clip created")
        nil)
    (nclip-add-clip-with-name name clip)))


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


(defun nclip-get-shortcut-clip (name)
  (gethash name *named-clips-shortcuts*))


(defun nclip-set-shortcut-clip (shortcut name)
  (puthash shortcut name *named-clips-shortcuts*))


(defun nclip-clip-names ()
  (let ((clip-names '()))
    (maphash (lambda (k _) (push k clip-names)) *named-clips-table*)
    clip-names))


(defun nclip-show-all-named-clips ()
  "Show a buffer that lists all the currently defined named clips and the content for each"
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
  "Copy the last thing you put on the kill ring and make it a named clip"
  (interactive "sName for clip: ")
  (let ((clip (substring-no-properties (current-kill 0 t))))
        (if (nclip-add-clip-with-name-check-exists name clip)
            (message (concat "Copied clip to " name)))))


(defun nclip-name-region-as-clip (name)
  "Copy the current region and make it a named clip"
  ;; TODO: need to check if region is even active, skip prompt and show message if not
  (interactive "sName for clip: ")
  (let ((clip (buffer-substring-no-properties (region-beginning) (region-end)))
        (mark (mark t))
        (point (point)))
    (when (nclip-add-clip-with-name-check-exists name clip)
      (message (concat "Copied region to " name))
      (deactivate-mark)
      (set-marker (mark-marker) (point) (current-buffer))
      (goto-char mark)
      (set-marker (mark-marker) mark (current-buffer))
      (goto-char point))))


(defun nclip-name-region-or-last-kill (name)
  "If there is a currently active region, make it a named clip, otherwise copy the last thing you \
put on the kill ring and make it a named clip."
  (interactive "sName for clip: ")
  (if (region-active-p) (nclip-name-region-as-clip name)
    (nclip-name-last-kill-text name)))


(defun nclip-insert-named-clip (name)
  "Insert a named clip at point in the current buffer."
  (interactive
   (list (completing-read "Name of clip: " (nclip-clip-names))))
  (insert (nclip-get-clip-with-name name))
  (nclip-set-shortcut-clip "last" name))


(defun nclip-put-named-clip-on-clipboard (name)
  "Put a named clip on the system clipboard and add it to the kill ring"
  (interactive
   (list (completing-read "Name of clip: " (nclip-clip-names))))
  (kill-new (nclip-get-clip-with-name name)))


(defun nclip-insert-last-named-clip ()
  "Insert the named clip that was last inserted"
  (interactive)
  (let ((last-clip (nclip-get-shortcut-clip "last")))
    (if last-clip (nclip-insert-named-clip last-clip)
      (message "No named clip has been used yet"))))


;; TODO: add quick shortcuts to refer to insert often-used clips, eg C-c n (0..9)
;; TODO: add way to assign quick shortcuts C-c n a (0..9)  (assign region or prompt for clipname)


(defun nclip-set-keybindings (&optional prefix)
  (let ((kb-prefix (or prefix named-clips-keybinding-prefix)))
    (global-set-key (kbd (concat kb-prefix " n")) 'nclip-name-region-or-last-kill)
    (global-set-key (kbd (concat kb-prefix " w")) 'nclip-name-region-as-clip)
    (global-set-key (kbd (concat kb-prefix " k")) 'nclip-name-last-kill-text)
    (global-set-key (kbd (concat kb-prefix " i")) 'nclip-insert-named-clip)
    (global-set-key (kbd (concat kb-prefix " c")) 'nclip-put-named-clip-on-clipboard)
    (global-set-key (kbd (concat kb-prefix " l")) 'nclip-show-all-named-clips)
    (global-set-key (kbd (concat kb-prefix " r")) 'nclip-remove-clip-with-name)
    (global-set-key (kbd (concat kb-prefix " q")) 'nclip-insert-last-named-clip)
    (global-set-key (kbd (concat kb-prefix " X")) 'nclip-clear-all-named-clips)))


(nclip-set-keybindings)
