(require 'seq)
(require 'cl-lib)

(define-button-type 'org-roam-node-button
  'follow-link t
  'help-echo "mouse-1, RET: Open this Org-roam note"
  'action (lambda (btn)
            (org-roam-node-open (button-get btn 'node)))
  'face `(:underline nil :foreground ,(doom-color 'blue)))

(defun org-roam-backlinks-count (node)
  "Return number of backlinks (other Org-roam notes linking to NODE)."
  (length (org-roam-backlinks-get node)))

(defun org-roam-link-counts (node)
  "Return a plist with :forward and :meta counts for NODE."
  (let* ((dest-ids
          (org-roam-db-query
           [:select [dest]
            :from links
            :where (= source $s1)]
           (org-roam-node-id node)))
         (forward 0)
         (meta 0))
    (dolist (row dest-ids)
      (let ((id (car row)))
        (if (org-roam-node-from-id id)
            (setq forward (1+ forward))
          (setq meta (1+ meta)))))
    (list :forward forward :meta meta)))

(defun butler/org-roam-dashboard ()
  "Dashboard of Org-roam notes sorted by timestamp in filename."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (all-tags (delete-dups (apply #'append (mapcar #'org-roam-node-tags all-nodes))))
         (all-types (delete-dups (mapcar #'org-roam-node-type all-nodes)))
         (tags (completing-read-multiple "Filter by tags (comma separated, RET for all): "
                                         all-tags nil t))
         (types (completing-read-multiple "Filter by types (comma separated, RET for all): "
                                          all-types nil t))
         ;; Filter by tags
         (nodes (if (seq-empty-p tags)
                    all-nodes
                  (seq-filter
                   (lambda (n)
                     (seq-some (lambda (tag) (member tag (org-roam-node-tags n)))
                               tags))
                   all-nodes)))
         ;; Filter by types (only if some selected)
         (nodes (if (seq-empty-p types)
                    nodes
                  (seq-filter (lambda (n)
                                (member (org-roam-node-type n) types))
                              nodes)))
         ;; Sort by timestamp in filename
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
      (insert (format "%-40s %-12s %-25s %-5s %-5s %-5s\n"
                      "Title" "Type" "Tags" "BL" "FL" "META"))
      (insert (make-string 100 ?-) "\n")
      (let ((bg (doom-color 'bg))
            (row-face `(:background ,(doom-color 'bg) :foreground ,(doom-color 'fg))))
        (dolist (node sorted-nodes)
          (let* ((title (org-roam-node-title node))
                 (title (if (> (length title) 40)
                            (concat (substring title 0 37) "…")
                          title))
                 ;; Show only selected tags in column (or all if none selected)
                 (tags-in-node (org-roam-node-tags node))
                 (tags-shown (if (seq-empty-p tags)
                                 tags-in-node
                               (seq-filter (lambda (tag) (member tag tags-in-node))
                                           tags)))
                 ;; fallback if no tags match
                 (tags-full (if (seq-empty-p tags-shown)
                                "-"
                              (string-join tags-shown ", ")))
                 (tags-str (if (> (length tags-full) 25)
                               (concat (substring tags-full 0 22) "…")
                             tags-full))
                 (type-str (or (org-roam-node-type node) ""))
                 (backlinks (org-roam-backlinks-count node))
                 (counts (org-roam-link-counts node))
                 (forwardlinks (plist-get counts :forward))
                 (metalinks (plist-get counts :meta)))
            (insert-text-button (format "%-40s" title)
                                'type 'org-roam-node-button
                                'node node
                                'face `(:underline nil :background ,bg :foreground ,(doom-color 'blue)))
            (insert (propertize
                     (format " %-12s %-25s %-5d %-5d %-5d\n"
                             type-str tags-str backlinks forwardlinks metalinks)
                     'face row-face)))))
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))

(defun butler/org-roam-untagged-dashboard ()
  "Dashboard of Org-roam notes that have no tags."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (nodes (seq-filter (lambda (n)
                              (seq-empty-p (org-roam-node-tags n)))
                            all-nodes))
         (sorted-nodes
          (seq-sort-by
           (lambda (node)
             (string-to-number
              (replace-regexp-in-string "\\D.*" ""
                (file-name-nondirectory (org-roam-node-file node)))))
           #'>
           nodes)))
    (with-current-buffer (get-buffer-create "*Org-roam Untagged Dashboard*")
      (erase-buffer)
      (insert (format "%-40s %-12s %-5s %-5s %-5s\n"
                      "Title" "Type" "BL" "FL" "META"))
      (insert (make-string 80 ?-) "\n")
      (let ((bg (doom-color 'bg))
            (row-face `(:background ,(doom-color 'bg) :foreground ,(doom-color 'fg))))
        (dolist (node sorted-nodes)
          (let* ((title (org-roam-node-title node))
                 (title (if (> (length title) 40)
                            (concat (substring title 0 37) "…")
                          title))
                 (type-str (or (org-roam-node-type node) ""))
                 (backlinks (org-roam-backlinks-count node))
                 (counts (org-roam-link-counts node))
                 (forwardlinks (plist-get counts :forward))
                 (metalinks (plist-get counts :meta)))
            (insert-text-button (format "%-40s" title)
                                'type 'org-roam-node-button
                                'node node
                                'face `(:underline nil :background ,bg :foreground ,(doom-color 'blue)))
            (insert (propertize
                     (format " %-12s %-5d %-5d %-5d\n"
                             type-str backlinks forwardlinks metalinks)
                     'face row-face)))))
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))
