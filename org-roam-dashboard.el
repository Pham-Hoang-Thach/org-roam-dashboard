(require 'seq)
(require 'cl-lib)

(define-button-type 'org-roam-node-button
  'follow-link t
  'help-echo "mouse-1, RET: Open this Org-roam note"
  'action (lambda (btn)
            (org-roam-node-open (button-get btn 'node)))
  ;; clickable title without underline, doom-nord blue
  'face `(:underline nil :foreground ,(doom-color 'blue)))

(defun org-roam-dashboard ()
  "Dashboard of Org-roam notes sorted by timestamp in filename.
Rows are colored alternately. Title clickable without underline.
Tags truncated with … if too long. Tooltip shows creation date/time."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (all-tags (delete-dups (apply #'append (mapcar #'org-roam-node-tags all-nodes))))
         (all-types (delete-dups (mapcar #'org-roam-node-type all-nodes)))
         (tags (completing-read-multiple "Filter by tags (comma separated, RET for all): "
                                         all-tags nil t))
         (types (completing-read-multiple "Filter by types (comma separated, RET for all): "
                                          all-types nil t))
         (tags (if (seq-empty-p tags) all-tags tags))
         (types (if (seq-empty-p types) all-types types))
         (nodes (seq-filter (lambda (n)
                              (seq-intersection tags (org-roam-node-tags n)))
                            all-nodes))
         (nodes (seq-filter (lambda (n)
                              (member (org-roam-node-type n) types))
                            nodes))
         ;; sort by timestamp in filename, newest first
         (sorted-nodes
          (seq-sort-by
           (lambda (node)
             (string-to-number
              (replace-regexp-in-string "\\D.*" ""
                (file-name-nondirectory (org-roam-node-file node)))))
           #'>
           nodes)))
    (with-current-buffer (get-buffer-create "*Org-roam Dashboard*")
      (erase-buffer)
      ;; header row
      (insert (format "%-60s %-15s %-40s %-10s\n"
                      "Title" "Type" "Tags" "Backlinks"))
      (insert (make-string 130 ?-) "\n")
      ;; rows with alternating colors
      (let ((row-index 0))
        (dolist (node sorted-nodes)
          (let* ((fname (file-name-nondirectory (org-roam-node-file node)))
                 ;; assume filename starts with YYYYMMDDHHMM
                 (ts-str (replace-regexp-in-string "\\D.*" "" fname))
                 (ts-date (ignore-errors
                            (format-time-string "%Y-%m-%d %H:%M"
                              (encode-time
                               0
                               (string-to-number (substring ts-str 10 12))
                               (string-to-number (substring ts-str 8 10))
                               (string-to-number (substring ts-str 6 8))
                               (string-to-number (substring ts-str 4 6))
                               (string-to-number (substring ts-str 0 4))))))
                 (title (org-roam-node-title node))
                 (title (if (> (length title) 60)
                            (concat (substring title 0 57) "…")
                          title))
                 (tags-full (string-join (org-roam-node-tags node) ", "))
                 ;; truncate tags to 40 chars with …
                 (tags-str (if (> (length tags-full) 40)
                               (concat (substring tags-full 0 37) "…")
                             tags-full))
                 (type-str (or (org-roam-node-type node) ""))
                 (backlinks (length (org-roam-backlinks-get node)))
                 ;; alternating background colors from doom-nord palette
                 (bg (if (cl-evenp row-index)
                         (doom-color 'bg-alt)
                       (doom-color 'bg)))
                 (row-face `(:background ,bg :foreground ,(doom-color 'fg))))
            ;; Title clickable, tooltip shows timestamp
            (insert-text-button (format "%-60s" title)
                                'type 'org-roam-node-button
                                'node node
                                'face `(:underline nil :background ,bg :foreground ,(doom-color 'blue))
                                'help-echo (or ts-date "No timestamp"))
            ;; Other columns with same row background
            (insert (propertize
                     (format " %-15s %-40s %-10d\n"
                             type-str tags-str backlinks)
                     'face row-face))
            (setq row-index (1+ row-index)))))
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))
